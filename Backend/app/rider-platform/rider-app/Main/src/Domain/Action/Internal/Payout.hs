module Domain.Action.Internal.Payout
  ( juspayPayoutWebhookHandler,
    payoutProcessingLockKey,
    castOrderStatus,
  )
where

import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.PersonStats as DPS
import qualified Domain.Types.VehicleCategory as DV
import Environment
import Kernel.Beam.Functions as B (runInReplica)
import qualified Kernel.External.Payout.Interface as Juspay
import qualified Kernel.External.Payout.Interface.Juspay as Juspay
import qualified Kernel.External.Payout.Interface.Types as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as Payout
import qualified Kernel.External.Payout.Types as TPayout
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import Servant (BasicAuthData)
import SharedLogic.Merchant
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPersonStats
import Tools.Error
import qualified Tools.Payout as Payout

payoutProcessingLockKey :: Text -> Text
payoutProcessingLockKey bookingId = "Payout:Processing:bookingId" <> bookingId

-- webhook ----------------------------------------------------------

juspayPayoutWebhookHandler ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  BasicAuthData ->
  Value ->
  Flow AckResponse
juspayPayoutWebhookHandler merchantShortId mbOpCity authData value = do
  merchant <- findMerchantByShortId merchantShortId
  merchanOperatingCityId <- CQMOC.getMerchantOpCityId merchant mbOpCity
  let merchantId = merchant.id
      serviceName' = DEMSC.PayoutService TPayout.Juspay
  merchantServiceConfig <-
    CQMSC.findByMerchantOpCityIdAndService merchantId merchanOperatingCityId serviceName'
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Payout" (show TPayout.Juspay))
  psc <- case merchantServiceConfig.serviceConfig of
    DMSC.PayoutServiceConfig psc' -> pure psc'
    _ -> throwError $ InternalError "Unknown Service Config"
  orderStatusResp <- Juspay.payoutOrderStatusWebhook psc authData value
  osr <- case orderStatusResp of
    Nothing -> throwError $ InternalError "Order Contents not found."
    Just osr' -> pure osr'
  logDebug $ "Webhook Payout Resp: " <> show osr
  case osr of
    IPayout.OrderStatusPayoutResp {..} -> do
      payoutOrder <- QPayoutOrder.findByOrderId payoutOrderId >>= fromMaybeM (PayoutOrderNotFound payoutOrderId)
      let personId = Id payoutOrder.customerId
      payoutConfig <- CPC.findByCityIdAndVehicleCategory merchanOperatingCityId DV.AUTO_CATEGORY Nothing >>= fromMaybeM (PayoutConfigNotFound "AUTO_CATEGORY" merchanOperatingCityId.getId)
      unless (isPayoutStatusSuccess payoutOrder.status) do
        case payoutOrder.entityName of
          Just DPayment.METRO_BOOKING_CASHBACK -> do
            forM_ (listToMaybe =<< payoutOrder.entityIds) $ \bookingId -> do
              when (isPayoutStatusSuccess payoutStatus) do
                QFTB.updatePayoutStatusById (Just $ castPayoutOrderStatus payoutStatus) (Id bookingId)
              fork "Update Payout Status and Transactions for MetroBooking" $ do
                callPayoutService payoutOrder payoutConfig
          Just DPayment.REFERRAL_AWARD_RIDE -> do
            when (isPayoutStatusSuccess payoutStatus) do
              personStats <- QPersonStats.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
              QPersonStats.updateReferralAmountPaid (personStats.referralAmountPaid + payoutOrder.amount.amount) personId
            fork "Update Payout Status and Transactions for Referral Award" $ do
              callPayoutService payoutOrder payoutConfig
          Just DPayment.REFERRED_BY_AWARD -> do
            when (isPayoutStatusSuccess payoutStatus) do
              QPersonStats.updateReferredByEarningsPayoutStatus (Just $ castOrderStatus payoutStatus) personId
            fork "Update Payout Status and Transactions for ReferredBy Award" $ do
              callPayoutService payoutOrder payoutConfig
          Just DPayment.BACKLOG -> do
            when (isPayoutStatusSuccess payoutStatus) do
              QPersonStats.updateBacklogPayoutStatus (Just $ castOrderStatus payoutStatus) personId
            fork "Update Payout Status and Transactions for Backlog Referral Award" $ do
              callPayoutService payoutOrder payoutConfig
          Just DPayment.REFERRED_BY_AND_BACKLOG_AWARD -> do
            when (isPayoutStatusSuccess payoutStatus) do
              let mbStatus = Just $ castOrderStatus payoutStatus
              QPersonStats.updateBacklogAndReferredByPayoutStatus mbStatus mbStatus personId
            fork "Update Payout Status and Transactions for Referred By And Backlog Award" $ do
              callPayoutService payoutOrder payoutConfig
          _ -> logTagError "Webhook Handler Error" $ "Unsupported Payout Entity:" <> show payoutOrder.entityName
    IPayout.BadStatusResp -> pure ()
  pure Ack
  where
    isPayoutStatusSuccess status = status `elem` [Payout.SUCCESS, Payout.FULFILLMENTS_SUCCESSFUL]

    callPayoutService payoutOrder payoutConfig = do
      let personId = Id payoutOrder.customerId
      person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      let createPayoutOrderStatusReq = IPayout.PayoutOrderStatusReq {orderId = payoutOrder.orderId, mbExpand = payoutConfig.expand}
          serviceName = DEMSC.PayoutService TPayout.Juspay
          createPayoutOrderStatusCall = Payout.payoutOrderStatus person.merchantId person.merchantOperatingCityId serviceName
      void $ DPayment.payoutStatusService (cast person.merchantId) (cast personId) createPayoutOrderStatusReq createPayoutOrderStatusCall

castPayoutOrderStatus :: Payout.PayoutOrderStatus -> DFTB.CashbackStatus
castPayoutOrderStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> DFTB.SUCCESSFUL
    Payout.FULFILLMENTS_SUCCESSFUL -> DFTB.SUCCESSFUL
    Payout.ERROR -> DFTB.CASHBACK_FAILED
    Payout.FAILURE -> DFTB.CASHBACK_FAILED
    Payout.FULFILLMENTS_FAILURE -> DFTB.CASHBACK_FAILED
    Payout.CANCELLED -> DFTB.MANUAL_VERIFICATION
    Payout.FULFILLMENTS_CANCELLED -> DFTB.MANUAL_VERIFICATION
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DFTB.MANUAL_VERIFICATION
    _ -> DFTB.PROCESSING

castOrderStatus :: Payout.PayoutOrderStatus -> DPS.PayoutStatus
castOrderStatus payoutOrderStatus =
  case payoutOrderStatus of
    Payout.SUCCESS -> DPS.Success
    Payout.FULFILLMENTS_SUCCESSFUL -> DPS.Success
    Payout.ERROR -> DPS.Failed
    Payout.FAILURE -> DPS.Failed
    Payout.FULFILLMENTS_FAILURE -> DPS.Failed
    Payout.CANCELLED -> DPS.Failed
    Payout.FULFILLMENTS_CANCELLED -> DPS.Failed
    Payout.FULFILLMENTS_MANUAL_REVIEW -> DPS.ManualReview
    _ -> DPS.Processing
