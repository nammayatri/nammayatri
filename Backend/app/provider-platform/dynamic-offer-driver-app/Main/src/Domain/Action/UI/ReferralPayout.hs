module Domain.Action.UI.ReferralPayout where

import qualified API.Types.UI.ReferralPayout
import Data.Text hiding (elem, filter, map)
import Data.Time.Calendar
import qualified Domain.Action.UI.Driver as DD
import qualified Domain.Action.UI.Payout as DAP
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PayoutConfig as DPC
import Domain.Types.Person
import qualified Domain.Types.VehicleCategory as DVC
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface.Types as KT
import qualified Kernel.External.Payment.Types as PaymentTypes
import qualified Kernel.External.Payout.Interface.Types as Payout
import qualified Kernel.External.Payout.Types as PT
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id (Id (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as Payout
import qualified Lib.Payment.Domain.Types.Common as DLP
import qualified Lib.Payment.Payout.Registration as Registration
import qualified Lib.Payment.Payout.Status as PayoutStatus
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.Queries.DailyStats as QDS
import qualified Storage.Queries.DailyStatsExtra as QDSE
import qualified Storage.Queries.DriverInformation as DrInfo
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import qualified Tools.Payment as TPayment
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
  let totalCount ern = ern.referralCounts + ern.d2dReferralCounts
      earnings = filter (\ern -> totalCount ern > 0) earnings_
  driverStats <- runInReplica $ QDriverStats.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  dInfo <- runInReplica $ DrInfo.findByPrimaryKey personId >>= fromMaybeM DriverInfoNotFound
  mbVehicle <- QVeh.findById personId
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
  payoutConfig <- CPC.findByPrimaryKey merchantOpCityId vehicleCategory Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchantOpCityId.getId)
  let dailyEarnings = map parseDailyEarnings earnings
  mbRegistrationOrder <- maybe (return Nothing) QOrder.findById (Id <$> dInfo.payoutRegistrationOrderId)
  let d2dEarnings = filter (\e -> e.d2dReferralCounts > 0) earnings
      d2cEarnings = filter (\e -> e.referralCounts > 0) earnings
      d2dDailyEarnings = map parseD2dDailyEarning d2dEarnings
      d2cDailyEarnings = map parseD2cDailyEarning d2cEarnings
  return $
    API.Types.UI.ReferralPayout.ReferralEarningsRes
      { totalReferralCount = driverStats.totalReferralCounts,
        d2dReferralCount = Just driverStats.d2dReferralCount,
        d2cReferralCount = Just driverStats.d2cReferralCount,
        dailyEarnings = dailyEarnings,
        vpaId = dInfo.payoutVpa,
        orderId = dInfo.payoutRegistrationOrderId,
        orderStatus = (.status) <$> mbRegistrationOrder,
        referralRewardAmountPerRide = payoutConfig.referralRewardAmountPerRide,
        payoutRegistrationAmount = sum [payoutConfig.payoutRegistrationFee, payoutConfig.payoutRegistrationCgst, payoutConfig.payoutRegistrationSgst],
        referralRewardAmountPerRideForD2DPayout = payoutConfig.referralRewardAmountPerRideForD2DPayout,
        d2dReferralEarnings = Just d2dDailyEarnings,
        d2cReferralEarnings = Just d2cDailyEarnings,
        payoutRegAmountRefunded = dInfo.payoutRegAmountRefunded,
        vpaVerificationMode = payoutConfig.vpaVerificationMode
      }
  where
    parseDailyEarnings earning =
      API.Types.UI.ReferralPayout.DailyEarning
        { earnings = earning.referralEarnings + earning.d2dReferralEarnings,
          activatedItems = earning.activatedValidRides + earning.d2dActivatedValidRides,
          earningDate = earning.merchantLocalDate,
          referrals = earning.referralCounts + earning.d2dReferralCounts,
          status = if (earning.payoutStatus `elem` [DS.PendingForVpa, DS.Initialized]) then DS.Verifying else earning.payoutStatus,
          payoutOrderId = earning.payoutOrderId
        }

    parseD2dDailyEarning earning =
      API.Types.UI.ReferralPayout.DailyEarning
        { earnings = earning.d2dReferralEarnings,
          activatedItems = earning.d2dActivatedValidRides,
          earningDate = earning.merchantLocalDate,
          referrals = earning.d2dReferralCounts,
          status = if (earning.payoutStatus `elem` [DS.PendingForVpa, DS.Initialized]) then DS.Verifying else earning.payoutStatus,
          payoutOrderId = earning.payoutOrderId
        }

    parseD2cDailyEarning earning =
      API.Types.UI.ReferralPayout.DailyEarning
        { earnings = earning.referralEarnings,
          activatedItems = earning.activatedValidRides,
          earningDate = earning.merchantLocalDate,
          referrals = earning.referralCounts,
          status = if (earning.payoutStatus `elem` [DS.PendingForVpa, DS.Initialized]) then DS.Verifying else earning.payoutStatus,
          payoutOrderId = earning.payoutOrderId
        }

postPayoutDeleteVpa ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postPayoutDeleteVpa (mbPersonId, _merchantId, merchantOpCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  driverInfo <- runInReplica $ DrInfo.findByPrimaryKey personId >>= fromMaybeM DriverInfoNotFound
  unless (isJust driverInfo.payoutVpa) $ throwError (InvalidRequest "Vpa Id does not Exists")
  void $ DrInfo.updatePayoutVpaAndStatus Nothing Nothing personId -- Deleting the prev VPA (We can get this in payout order history)
  -- Reset refund tracking so re-registration (₹2 payment) can be refunded again
  mbVehicle <- QVeh.findById personId
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
  mbPayoutConfig <- CPC.findByPrimaryKey merchantOpCityId vehicleCategory Nothing
  when (maybe False (\pc -> pc.vpaVerificationMode == DPC.PAYMENT_BASED) mbPayoutConfig) $
    DrInfo.updatePayoutRegAmountRefunded Nothing personId
  pure Kernel.Types.APISuccess.Success

postPayoutUpdateVpa ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.ReferralPayout.UpdatePayoutVpaReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postPayoutUpdateVpa (mbPersonId, _merchantId, merchantOpCityId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  mbVehicle <- QVeh.findById personId
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
  payoutConfig <- CPC.findByPrimaryKey merchantOpCityId vehicleCategory Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchantOpCityId.getId)
  unless payoutConfig.isPayoutEnabled $ throwError $ InvalidRequest "Payout is Not Enabled"
  unless (payoutConfig.vpaVerificationMode == DPC.API_BASED) $
    throwError $ InvalidRequest "VPA update via API is not enabled. Please use the registration payment flow."
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- Verify VPA using Juspay API
  paymentServiceName <- TPayment.decidePaymentService (DEMSC.PaymentService PaymentTypes.Juspay) person.clientSdkVersion person.merchantOperatingCityId
  let verifyVPAReq =
        KT.VerifyVPAReq
          { orderId = Nothing,
            customerId = Just person.id.getId,
            vpa = req.vpa
          }
      verifyVpaCall = TPayment.verifyVpa person.merchantId person.merchantOperatingCityId paymentServiceName (Just person.id.getId)
  resp <- try @_ @SomeException $ Payout.verifyVPAService verifyVPAReq verifyVpaCall
  case resp of
    Left e -> throwError $ InvalidRequest $ "VPA Verification Failed: " <> show e
    Right response ->
      unless (response.status == "VALID") $
        throwError $ InvalidRequest $ "Invalid VPA: " <> req.vpa
  -- VPA is valid — store it
  DrInfo.updatePayoutVpaAndStatus (Just req.vpa) (Just DI.VERIFIED_BY_USER) personId
  -- Process backlog referral payouts with the just-verified VPA.
  -- processPreviousPayoutAmount uses tryLockRedis (non-blocking), so concurrent
  -- calls from rapid VPA updates are dropped — no risk of double-pay.
  fork ("processing backlog payout for driver via updateVpa " <> personId.getId) $
    DAP.processPreviousPayoutAmount personId (Just req.vpa) merchantOpCityId
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
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
  payoutConfig <- CPC.findByPrimaryKey merchantOpCityId vehicleCategory Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchantOpCityId.getId)
  unless payoutConfig.isPayoutEnabled $ throwError $ InvalidRequest "Payout Registration is Not Enabled"
  when (payoutConfig.vpaVerificationMode == DPC.API_BASED) $
    throwError $ InvalidRequest "Use /payout/update/vpa to register your VPA"

  -- Get driver details for creating the payment order
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  let driverEmail = fromMaybe "test@juspay.in" person.email

  -- Get Juspay createOrder call
  paymentServiceName <- TPayment.decidePaymentService (DEMSC.PaymentService PaymentTypes.Juspay) person.clientSdkVersion person.merchantOperatingCityId
  (createOrderCall, _pseudoClientId) <- TPayment.createOrder merchantId merchantOpCityId paymentServiceName (Just person.id.getId)

  -- Delegate to lib
  regResult <-
    Registration.initiateRegistration
      (Kernel.Types.Id.cast merchantId)
      (Just $ Kernel.Types.Id.cast merchantOpCityId)
      (Kernel.Types.Id.cast personId)
      createOrderCall
      driverPhone
      driverEmail
      (Just person.firstName)
      person.lastName

  -- Store orderId on DriverInformation
  DrInfo.updatePayoutRegistrationOrderId (Just regResult.orderId.getId) personId
  pure $
    DD.ClearDuesRes
      { orderId = Kernel.Types.Id.cast regResult.orderId,
        orderResp = regResult.orderResp
      }

postPayoutCreateOrder ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Payout.CreatePayoutOrderReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postPayoutCreateOrder (mbPersonId, merchantId, merchantOpCityId) req = do
  void $ throwError $ InvalidRequest "You're Not Authorized To Use This API"
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (InvalidRequest "Person not found")
  payoutServiceName <- TP.decidePayoutService (DEMSC.PayoutService PT.Juspay) person.clientSdkVersion person.merchantOperatingCityId
  let entityName = DLP.MANUAL
      createPayoutOrderCall = TP.createPayoutOrder merchantId merchantOpCityId payoutServiceName (Just person.id.getId)
  merchantOperatingCity <- CQMOC.findById (Kernel.Types.Id.cast merchantOpCityId) >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  void $ Payout.createPayoutService (Kernel.Types.Id.cast merchantId) (Just $ Kernel.Types.Id.cast merchantOpCityId) (Kernel.Types.Id.cast personId) (Just [personId.getId]) (Just entityName) (show merchantOperatingCity.city) req createPayoutOrderCall Nothing
  pure Kernel.Types.APISuccess.Success

getPayoutOrderStatus ::
  (EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, CacheFlow m r, MonadFlow m, HasShortDurationRetryCfg r c, ServiceFlow m r) =>
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    m Payout.PayoutOrderStatusResp
  )
getPayoutOrderStatus (mbPersonId, merchantId, merchantOpCityId) orderId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (InvalidRequest "Person not found")
  payoutOrder <- QPayoutOrder.findByOrderId orderId >>= fromMaybeM (PayoutOrderNotFound orderId)
  mbVehicle <- QVeh.findById personId
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
  payoutConfig <- CPC.findByPrimaryKey merchantOpCityId vehicleCategory Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchantOpCityId.getId)
  payoutServiceName <- TP.decidePayoutService (DEMSC.PayoutService PT.Juspay) person.clientSdkVersion person.merchantOperatingCityId
  let payoutOrderStatusReq = Payout.PayoutOrderStatusReq {orderId = orderId, mbExpand = payoutConfig.expand}
      shouldUpdate current new = current /= new
      onUpdate newStatus _statusResp = do
        when (maybe False (`elem` [DLP.DRIVER_DAILY_STATS, DLP.BACKLOG]) payoutOrder.entityName) do
          whenJust payoutOrder.entityIds $ \dStatsIds -> do
            forM_ dStatsIds $ \dStatsId -> do
              Redis.withWaitOnLockRedisWithExpiry (DAP.payoutProcessingLockKey personId.getId) 3 3 $ do
                QDS.updatePayoutStatusById newStatus dStatsId
  PayoutStatus.refreshPayoutStatusWithResponse
    orderId
    payoutOrderStatusReq
    (TP.payoutOrderStatus merchantId merchantOpCityId payoutServiceName (Just $ getId personId))
    (DAP.castPayoutOrderStatus payoutOrder.status)
    DAP.castPayoutOrderStatus
    shouldUpdate
    onUpdate
