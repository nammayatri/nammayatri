{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.ReferralPayout where

import qualified API.Types.UI.ReferralPayout
import Data.OpenApi (ToSchema)
import Data.Text hiding (filter, map)
import Data.Time.Calendar
import qualified Domain.Action.UI.Driver as DD
import qualified Domain.Action.UI.Payout as DAP
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.DriverFee as DFee
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.Vehicle as DV
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import qualified Kernel.External.Payout.Interface.Types as Payout
import qualified Kernel.External.Payout.Types as PT
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Error
import Kernel.Types.Id (Id (..))
import qualified Kernel.Types.Id
import qualified Kernel.Types.Price (Currency (..))
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as Payout
import qualified Lib.Payment.Domain.Types.Common as DLP
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.Queries.DailyStats as QDS
import qualified Storage.Queries.DailyStatsExtra as QDSE
import qualified Storage.Queries.DriverInformation as DrInfo
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as PersonQuery
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Auth
import Tools.Error
import qualified Tools.Payout as TP

getPayoutReferralEarnings ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Time.Calendar.Day ->
    Data.Time.Calendar.Day ->
    Environment.Flow API.Types.UI.ReferralPayout.ReferralEarningsRes
  )
getPayoutReferralEarnings (mbPersonId, _merchantId, merchantOpCityId) fromDate toDate = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  earnings_ <- QDSE.findAllInRangeByDriverId_ personId fromDate toDate
  let earnings = filter (\ern -> ern.referralCounts > 0) earnings_
  driverStats <- runInReplica $ QDriverStats.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  dInfo <- runInReplica $ DrInfo.findByPrimaryKey personId >>= fromMaybeM DriverInfoNotFound
  mbVehicle <- QVeh.findById personId
  let vehicleCategory = fromMaybe DV.AUTO_CATEGORY ((.category) =<< mbVehicle)
  payoutConfig <- CPC.findByPrimaryKey merchantOpCityId vehicleCategory >>= fromMaybeM (InternalError "Payout config not present")
  let dailyEarnings = map parseDailyEarnings earnings
  mbRegistrationOrder <- maybe (return Nothing) QOrder.findById (Id <$> dInfo.payoutRegistrationOrderId)
  return $
    API.Types.UI.ReferralPayout.ReferralEarningsRes
      { totalReferralCount = driverStats.totalReferralCounts,
        dailyEarnings = dailyEarnings,
        vpaId = dInfo.payoutVpa,
        orderId = (.shortId.getShortId) <$> mbRegistrationOrder,
        orderStatus = (.status) <$> mbRegistrationOrder,
        referralRewardAmountPerRide = payoutConfig.referralRewardAmountPerRide,
        payoutRegistrationAmount = sum [payoutConfig.payoutRegistrationFee, payoutConfig.payoutRegistrationCgst, payoutConfig.payoutRegistrationSgst]
      }
  where
    parseDailyEarnings earning =
      API.Types.UI.ReferralPayout.DailyEarning
        { earnings = earning.referralEarnings,
          activatedItems = earning.activatedValidRides,
          earningDate = earning.merchantLocalDate,
          referrals = earning.referralCounts,
          status = earning.payoutStatus,
          payoutOrderId = earning.payoutOrderId
        }

postPayoutDeleteVpa ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postPayoutDeleteVpa (mbPersonId, _merchantId, _merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  driverInfo <- runInReplica $ DrInfo.findByPrimaryKey personId >>= fromMaybeM DriverInfoNotFound
  unless (isJust driverInfo.payoutVpa) $ throwError (InvalidRequest "Vpa Id does not Exists")
  void $ DrInfo.updatePayoutVpa Nothing personId -- Deleting the prev VPA (We can get this in payout order history)
  pure Kernel.Types.APISuccess.Success

getPayoutRegistration ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow DD.ClearDuesRes
  )
getPayoutRegistration (mbPersonId, merchantId, merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  mbVehicle <- QVeh.findById personId
  let vehicleCategory = fromMaybe DV.AUTO_CATEGORY ((.category) =<< mbVehicle)
  payoutConfig <- CPC.findByPrimaryKey merchantOpCityId vehicleCategory >>= fromMaybeM (InternalError "Payout config not present")
  unless payoutConfig.isPayoutEnabled $ throwError $ InvalidRequest "Payout Registration is Not Enabled"
  let (fee, cgst, sgst) = (payoutConfig.payoutRegistrationFee, payoutConfig.payoutRegistrationCgst, payoutConfig.payoutRegistrationSgst)
  clearDuesRes <- DD.clearDriverFeeWithCreate (personId, merchantId, merchantOpCityId) DPlan.YATRI_SUBSCRIPTION (fee, cgst, sgst) DFee.PAYOUT_REGISTRATION INR Nothing
  DrInfo.updatePayoutRegistrationOrderId (Just clearDuesRes.orderId.getId) personId
  pure clearDuesRes

postPayoutCreateOrder ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Payout.CreatePayoutOrderReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postPayoutCreateOrder (mbPersonId, merchantId, merchantOpCityId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  let serviceName = DEMSC.PayoutService PT.Juspay
  let entityName = DLP.MANUAL
      createPayoutOrderCall = TP.createPayoutOrder merchantId merchantOpCityId serviceName
  merchantOperatingCity <- CQMOC.findById (Kernel.Types.Id.cast merchantOpCityId) >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  void $ Payout.createPayoutService (Kernel.Types.Id.cast merchantId) (Kernel.Types.Id.cast personId) (Just [personId.getId]) (Just entityName) (show merchantOperatingCity.city) req createPayoutOrderCall
  pure Kernel.Types.APISuccess.Success

getPayoutOrderStatus ::
  (EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, CacheFlow m r, MonadFlow m, HasShortDurationRetryCfg r c, ServiceFlow m r) =>
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Data.Text.Text ->
    m Payout.PayoutOrderStatusResp
  )
getPayoutOrderStatus (mbPersonId, merchantId, merchantOpCityId) mbDailyStatsId orderId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  _payoutOrder <- QPayoutOrder.findByOrderId orderId >>= fromMaybeM (PayoutOrderNotFound orderId) -- validation Of OrderId
  let payoutOrderStatusReq = Payout.PayoutOrderStatusReq {orderId = orderId}
      serviceName = DEMSC.PayoutService PT.Juspay
  statusResp <- TP.payoutOrderStatus merchantId merchantOpCityId serviceName payoutOrderStatusReq
  Payout.payoutStatusUpdates statusResp.status orderId (Just statusResp)
  whenJust mbDailyStatsId $ \dStatsId -> do
    Redis.withWaitOnLockRedisWithExpiry (DAP.payoutProcessingLockKey personId.getId) 3 3 $ do
      let dPayoutStatus = DAP.castPayoutOrderStatus statusResp.status
      QDS.updatePayoutStatusById dPayoutStatus dStatsId
  pure statusResp