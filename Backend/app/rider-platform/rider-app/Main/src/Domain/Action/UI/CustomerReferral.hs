module Domain.Action.UI.CustomerReferral where

import API.Types.UI.CustomerReferral
import qualified Domain.Action.Beckn.Common as Common
import qualified Domain.Action.Internal.Payout as DPayout
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as MerchantOpCity
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonStats as PS
import qualified Domain.Types.VehicleCategory as VehicleCategory
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface.Types as KT
import qualified Kernel.External.Payout.Types as PT
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DP
import qualified Lib.Payment.Domain.Action as Payout
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified SharedLogic.Referral as Referral
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as PStats
import Tools.Error
import qualified Tools.Payment as TPayment
import qualified Tools.Payout as TPayout

getCustomerRefferalCount :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Flow ReferredCustomers
getCustomerRefferalCount (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  stats <- PStats.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
  pure $ ReferredCustomers {count = stats.referralCount}

postPersonApplyReferral :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> ApplyCodeReq -> Flow ReferrerInfo
postPersonApplyReferral (mbPersonId, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  res <- Referral.applyReferralCode person shouldShareReferrerInfo req.code
  let mbAndroidId = bool req.androidId Nothing (isJust person.androidId)
      mbDeviceId = bool req.deviceId Nothing (isJust person.deviceId)
  void $ QPerson.updateAndroidIdAndDeviceId personId mbAndroidId mbDeviceId
  case res of
    Left success ->
      -- NOTE: Error shouldn't come if driver app is released with the latest version & before that this api shouldn't be used.
      throwError . InternalError $ "Expected to have referrerInfo but got success: " <> show success
    Right referrerInfo -> pure referrerInfo
  where
    shouldShareReferrerInfo = True

getReferralVerifyVpa :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Kernel.Prelude.Text -> Flow VpaResp
getReferralVerifyVpa (mbPersonId, _mbMerchantId) vpa = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let verifyVPAReq =
        KT.VerifyVPAReq
          { orderId = Nothing,
            customerId = Just personId.getId,
            vpa = vpa
          }
      commonMerchantId = cast @Merchant.Merchant @DPayment.Merchant person.merchantId
      commonPersonId = cast @Person.Person @DPayment.Person personId
      verifyVpaCall = TPayment.verifyVpa person.merchantId person.merchantOperatingCityId Nothing TPayment.Normal
  resp <- try @_ @SomeException $ DP.verifyVPAService commonMerchantId commonPersonId verifyVPAReq verifyVpaCall
  case resp of
    Left e -> throwError $ InvalidRequest $ "VPA Verification Failed: " <> show e
    Right response -> do
      logDebug $ "verify vpa resp : " <> show response
      pure $ VpaResp {vpa = response.vpa, isValid = response.status == "VALID"}

getReferralPayoutHistory :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Flow PayoutHistory
getReferralPayoutHistory (mbPersonId, _mbMerchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  payoutOrders <- QPayoutOrder.findAllByCustomerId personId.getId
  let history = map getPayoutOrderDetails payoutOrders
  pure $ PayoutHistory {..}
  where
    getPayoutOrderDetails payoutOrder =
      PayoutItem
        { amount = payoutOrder.amount.amount,
          vpa = payoutOrder.vpa,
          payoutAt = payoutOrder.createdAt,
          payoutStatus = DPayout.castOrderStatus payoutOrder.status,
          orderId = payoutOrder.orderId
        }

postPayoutVpaUpsert :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> UpdatePayoutVpaReq -> Flow APISuccess
postPayoutVpaUpsert (mbPersonId, _mbMerchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  QPerson.updatePayoutVpa (Just req.vpa) personId
  fork ("processing backlog payout for customer while vpa updation" <> personId.getId) $
    processBacklogReferralPayout personId req.vpa person.merchantOperatingCityId
  pure Success

processBacklogReferralPayout ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r
  ) =>
  Id Person.Person ->
  Text ->
  Id MerchantOpCity.MerchantOperatingCity ->
  m ()
processBacklogReferralPayout personId vpa merchantOpCityId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbPayoutConfig <- CPC.findByCityIdAndVehicleCategory person.merchantOperatingCityId VehicleCategory.AUTO_CATEGORY Nothing
  personStats <- PStats.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
  let toPayReferredByReward = personStats.referredByEarnings > 0 && isNothing personStats.referredByEarningsPayoutStatus
      toPayBacklogAmount = personStats.backlogPayoutAmount > 0 && isNothing personStats.backlogPayoutStatus
  when (toPayReferredByReward || toPayBacklogAmount) $ do
    Redis.withWaitOnLockRedisWithExpiry (Common.payoutProcessingLockKey personId.getId) 3 3 $ do
      let amount = (bool 0 personStats.backlogPayoutAmount toPayBacklogAmount) + (bool 0 personStats.referredByEarnings toPayReferredByReward)
          entityName = getEntityName toPayReferredByReward toPayBacklogAmount
      case entityName of
        DPayment.REFERRED_BY_AND_BACKLOG_AWARD -> PStats.updateBacklogAndReferredByPayoutStatus (Just PS.Processing) (Just PS.Processing) personId
        DPayment.REFERRED_BY_AWARD -> PStats.updateReferredByEarningsPayoutStatus (Just PS.Processing) personId
        DPayment.BACKLOG -> PStats.updateBacklogPayoutStatus (Just PS.Processing) personId
        _ -> pure ()
      handlePayout person amount mbPayoutConfig entityName
  where
    handlePayout person amount mbPayoutConfig entityName = do
      case mbPayoutConfig of
        Just payoutConfig -> do
          phoneNo <- mapM decrypt person.mobileNumber
          emailId <- mapM decrypt person.email
          uid <- generateGUID
          let createPayoutOrderReq = Payout.mkCreatePayoutOrderReq uid amount phoneNo emailId person.id.getId payoutConfig.remark person.firstName vpa payoutConfig.orderType
          logDebug $ "create payoutOrder with riderId: " <> person.id.getId <> " | amount: " <> show amount <> " | orderId: " <> show uid
          let serviceName = DEMSC.PayoutService PT.Juspay
              createPayoutOrderCall = TPayout.createPayoutOrder person.merchantId merchantOpCityId serviceName
          merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
          void $ try @_ @SomeException $ Payout.createPayoutService (cast person.merchantId) (Just $ cast merchantOpCityId) (cast person.id) (Just []) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall
        Nothing -> logTagError "Payout Config Error" $ "PayoutConfig Not Found During Backlog Payout for cityId: " <> merchantOpCityId.getId

    getEntityName toPayReferredByReward toPayBacklogAmount = case (toPayReferredByReward, toPayBacklogAmount) of
      (True, True) -> DPayment.REFERRED_BY_AND_BACKLOG_AWARD
      (True, False) -> DPayment.REFERRED_BY_AWARD
      (False, _) -> DPayment.BACKLOG
