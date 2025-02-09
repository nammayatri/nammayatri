{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE FlexibleInstances #-}

module Domain.Action.UI.Plan where

import Data.List (intersect, nubBy)
import qualified Data.List as DL
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.OpenApi (ToSchema (..))
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverPlan
import qualified Domain.Types.Invoice as Domain
import qualified Domain.Types.Invoice as INV
import Domain.Types.Mandate (MandateStatus)
import qualified Domain.Types.Mandate as DM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as MessageKey
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Domain.Types.Plan as P
import Domain.Types.SubscriptionConfig
import Domain.Types.TransporterConfig (TransporterConfig)
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleVariant as Vehicle
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types (Language (ENGLISH))
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Storage.Queries.PaymentOrder as SOrder
import SharedLogic.DriverFee (calcNumRides, calculatePlatformFeeAttr, getPaymentModeAndVehicleCategoryKey, getStartTimeAndEndTimeRange, mkCachedKeyTotalRidesByDriverId, roundToHalf)
import qualified SharedLogic.Merchant as SMerchant
import SharedLogic.Payment
import qualified SharedLogic.Payment as SPayment
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Plan as QPD
import qualified Storage.CachedQueries.PlanTranslation as CQPTD
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as DI
import qualified Storage.Queries.DriverPlan as QDPlan
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.Mandate as QM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VendorFee as QVF
import Tools.Error
import Tools.Notifications
import Tools.Payment as Payment
import Utils.Common.Cac.KeyNameConstants

---------------------------------------------------------------------------------------------------------
--------------------------------------- Request & Response Types ----------------------------------------
---------------------------------------------------------------------------------------------------------

data PlanListAPIRes = PlanListAPIRes
  { list :: [PlanEntity],
    subscriptionStartTime :: UTCTime,
    isLocalized :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PlanEntity = PlanEntity
  { id :: Text,
    name :: Text,
    description :: Text,
    planFareBreakup :: [PlanFareBreakup],
    freeRideCount :: Int,
    frequency :: Text,
    offers :: [OfferEntity],
    paymentMode :: PaymentMode,
    totalPlanCreditLimit :: Money,
    currentDues :: HighPrecMoney,
    autopayDues :: HighPrecMoney,
    dueBoothCharges :: HighPrecMoney,
    totalPlanCreditLimitWithCurrency :: PriceAPIEntity,
    currentDuesWithCurrency :: PriceAPIEntity,
    autopayDuesWithCurrency :: PriceAPIEntity,
    dueBoothChargesWithCurrency :: PriceAPIEntity,
    dues :: [DriverDuesEntity],
    coinEntity :: Maybe CoinEntity,
    bankErrors :: [ErrorEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ErrorEntity = ErrorEntity
  { message :: Text,
    code :: Text,
    amount :: HighPrecMoney,
    amountWithCurrency :: PriceAPIEntity
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PlanFareBreakup = PlanFareBreakup
  { component :: Text,
    amount :: HighPrecMoney,
    amountWithCurrency :: PriceAPIEntity
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data OfferEntity = OfferEntity
  { title :: Maybe Text,
    description :: Maybe Text,
    tnc :: Maybe Text,
    offerId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CurrentPlanRes = CurrentPlanRes
  { currentPlanDetails :: Maybe PlanEntity,
    mandateDetails :: Maybe MandateDetailsEntity,
    orderId :: Maybe (Id DOrder.PaymentOrder),
    lastPaymentType :: Maybe PaymentType,
    autoPayStatus :: Maybe DI.DriverAutoPayStatus,
    subscribed :: Bool,
    planRegistrationDate :: Maybe UTCTime,
    latestAutopayPaymentDate :: Maybe UTCTime,
    latestManualPaymentDate :: Maybe UTCTime,
    isEligibleForCharge :: Maybe Bool,
    isLocalized :: Maybe Bool,
    payoutVpa :: Maybe Text,
    askForPlanSwitchByCity :: Bool,
    askForPlanSwitchByVehicle :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data CoinEntity = CoinEntity
  { coinDiscountUpto :: HighPrecMoney,
    coinDiscountUptoWithCurrency :: PriceAPIEntity
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PaymentType = CLEAR_DUE | AUTOPAY_REGISTRATION deriving (Generic, ToJSON, FromJSON, ToSchema)

data PlanSubscribeRes = PlanSubscribeRes
  { orderId :: Id DOrder.PaymentOrder,
    orderResp :: Payment.CreateOrderResp
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MandateDetailsEntity = MandateDetails
  { status :: MandateStatus,
    startDate :: UTCTime,
    endDate :: UTCTime,
    mandateId :: Text,
    payerVpa :: Maybe Text,
    payerApp :: Maybe Text,
    frequency :: Text,
    maxAmount :: Money,
    maxAmountWithCurrency :: PriceAPIEntity,
    autopaySetupDate :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverDuesEntity = DriverDuesEntity
  { createdAt :: Maybe UTCTime,
    executionAt :: Maybe UTCTime,
    feeType :: DF.FeeType,
    autoPayStage :: Maybe DF.AutopayPaymentStage,
    paymentStatus :: Maybe INV.InvoiceStatus,
    totalEarnings :: HighPrecMoney,
    totalEarningsWithCurrency :: PriceAPIEntity,
    rideTakenOn :: UTCTime,
    driverFeeAmount :: HighPrecMoney,
    driverFeeAmountWithCurrency :: PriceAPIEntity,
    totalRides :: Int,
    planAmount :: HighPrecMoney,
    planAmountWithCurrency :: PriceAPIEntity,
    isSplit :: Bool,
    offerAndPlanDetails :: Maybe Text,
    isCoinCleared :: Bool,
    maxRidesEligibleForCharge :: Maybe Int,
    coinDiscountAmount :: Maybe HighPrecMoney,
    coinDiscountAmountWithCurrency :: Maybe PriceAPIEntity,
    specialZoneRideCount :: Int,
    totalSpecialZoneCharges :: HighPrecMoney,
    totalSpecialZoneChargesWithCurrency :: PriceAPIEntity,
    driverFeeId :: Text,
    status :: DF.DriverFeeStatus
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

data ServicesEntity = ServicesEntity
  { services :: [ServiceNames]
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

planServiceLists ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m ServicesEntity
planServiceLists (driverId, _, merchantOpCityId) = do
  subscriptionConfigs <- CQSC.findAllServicesUIEnabledByCity merchantOpCityId True
  driverInfo <- DI.findById (cast driverId)
  let driverEnabledForServices = maybe [] (.servicesEnabledForSubscription) driverInfo
  let commonServices = DL.intersect driverEnabledForServices (map (.serviceName) subscriptionConfigs)
  return $ ServicesEntity {services = commonServices}

class Subscription a where
  getSubcriptionStatusWithPlan :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => a -> Id SP.Person -> m (Maybe DI.DriverAutoPayStatus, Maybe DriverPlan)
  updateSubscriptionStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => a -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe DI.DriverAutoPayStatus -> Maybe Text -> m ()
  createDriverPlan :: a -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Plan -> SubscriptionServiceRelatedData -> Flow ()
  planSubscribe :: a -> Id Plan -> (Bool, Maybe MessageKey.MediaChannel) -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> DI.DriverInformation -> SubscriptionServiceRelatedData -> Flow PlanSubscribeRes
  planSwitch :: a -> Id Plan -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow APISuccess
  planSuspend :: a -> Bool -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow APISuccess
  planResume :: a -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow APISuccess
  getSubscriptionConfigAndPlan :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => a -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe DriverPlan -> m (SubscriptionConfig, [Plan])

instance Subscription ServiceNames where
  getSubcriptionStatusWithPlan serviceName driverId = do
    case serviceName of
      YATRI_SUBSCRIPTION -> getSubcriptionStatusWithPlanGeneric YATRI_SUBSCRIPTION driverId
      YATRI_RENTAL -> getSubcriptionStatusWithPlanGeneric YATRI_RENTAL driverId
      DASHCAM_RENTAL _ -> getSubcriptionStatusWithPlanGeneric serviceName driverId
  createDriverPlan serviceName (driverId, merchantId, opCity) plan subscriptionServiceRelatedData = do
    case serviceName of
      YATRI_SUBSCRIPTION -> createDriverPlanGeneric YATRI_SUBSCRIPTION (driverId, merchantId, opCity) plan subscriptionServiceRelatedData
      YATRI_RENTAL -> createDriverPlanGeneric YATRI_RENTAL (driverId, merchantId, opCity) plan subscriptionServiceRelatedData
      DASHCAM_RENTAL _ -> createDriverPlanGeneric serviceName (driverId, merchantId, opCity) plan subscriptionServiceRelatedData
  planSubscribe serviceName = do
    case serviceName of
      YATRI_SUBSCRIPTION -> planSubscribeGeneric YATRI_SUBSCRIPTION
      YATRI_RENTAL -> planSubscribeGeneric YATRI_RENTAL
      DASHCAM_RENTAL _ -> planSubscribeGeneric serviceName
  planSwitch serviceName = do
    case serviceName of
      YATRI_SUBSCRIPTION -> planSwitchGeneric YATRI_SUBSCRIPTION
      YATRI_RENTAL -> planSwitchGeneric YATRI_RENTAL
      DASHCAM_RENTAL _ -> planSwitchGeneric serviceName
  planSuspend serviceName = do
    case serviceName of
      YATRI_SUBSCRIPTION -> planSuspendGeneric YATRI_SUBSCRIPTION
      YATRI_RENTAL -> planSuspendGeneric YATRI_RENTAL
      DASHCAM_RENTAL _ -> planSuspendGeneric serviceName
  planResume serviceName = do
    case serviceName of
      YATRI_SUBSCRIPTION -> planResumeGeneric YATRI_SUBSCRIPTION
      YATRI_RENTAL -> planResumeGeneric YATRI_RENTAL
      DASHCAM_RENTAL _ -> planResumeGeneric serviceName
  updateSubscriptionStatus serviceName (driverId, merchantId, opCity) autoPayStatus mbPayerVpa = do
    case serviceName of
      YATRI_SUBSCRIPTION -> updateSubscriptionStatusGeneric YATRI_SUBSCRIPTION (driverId, merchantId, opCity) autoPayStatus mbPayerVpa
      YATRI_RENTAL -> updateSubscriptionStatusGeneric YATRI_RENTAL (driverId, merchantId, opCity) autoPayStatus mbPayerVpa
      DASHCAM_RENTAL _ -> updateSubscriptionStatusGeneric serviceName (driverId, merchantId, opCity) autoPayStatus mbPayerVpa
  getSubscriptionConfigAndPlan serviceName (driverId, merchantId, opCity) mbDPlan = do
    case serviceName of
      YATRI_SUBSCRIPTION -> getSubsriptionConfigAndPlanSubscription YATRI_SUBSCRIPTION (driverId, merchantId, opCity) mbDPlan
      YATRI_RENTAL -> getSubsriptionConfigAndPlanGeneric YATRI_RENTAL (driverId, merchantId, opCity) mbDPlan
      DASHCAM_RENTAL _ -> getSubsriptionConfigAndPlanGeneric serviceName (driverId, merchantId, opCity) mbDPlan

---------------------------------------------------------------------------------------------------------
--------------------------------------------- Controllers -----------------------------------------------
---------------------------------------------------------------------------------------------------------

getSubcriptionStatusWithPlanGeneric ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  ServiceNames ->
  Id SP.Person ->
  m (Maybe DI.DriverAutoPayStatus, Maybe DriverPlan)
getSubcriptionStatusWithPlanGeneric serviceName driverId = do
  driverPlan <- QDPlan.findByDriverIdWithServiceName driverId serviceName
  let autoPayStatusFromDPlan = driverPlan >>= (.autoPayStatus)
  autoPayStatus <- do
    case (driverPlan, autoPayStatusFromDPlan, serviceName) of
      (Just _, Nothing, YATRI_SUBSCRIPTION) -> do
        driverInfo <- DI.findById (cast driverId)
        return $ driverInfo >>= (.autoPayStatus)
      (_, _, _) -> return autoPayStatusFromDPlan
  return (autoPayStatus, driverPlan)

updateSubscriptionStatusGeneric ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  ServiceNames ->
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe DI.DriverAutoPayStatus ->
  Maybe Text ->
  m ()
updateSubscriptionStatusGeneric serviceName (driverId, _, _) stage mbPayerVpa = do
  QDPlan.updateAutoPayStatusAndPayerVpaByDriverIdAndServiceName driverId serviceName stage mbPayerVpa

createDriverPlanGeneric ::
  ServiceNames ->
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Plan ->
  SubscriptionServiceRelatedData ->
  Flow ()
createDriverPlanGeneric serviceName (driverId, merchantId, merchantOpCityId) plan subscriptionServiceRelatedData = do
  now <- getCurrentTime
  let dPlan =
        DriverPlan
          { driverId = cast driverId,
            planId = plan.id,
            planType = plan.paymentMode,
            mandateId = Nothing,
            createdAt = now,
            updatedAt = now,
            mandateSetupDate = Nothing,
            coinCovertedToCashLeft = 0.0,
            totalCoinsConvertedCash = 0.0,
            serviceName = serviceName,
            autoPayStatus = Just DI.PENDING,
            enableServiceUsageCharge = True,
            subscriptionServiceRelatedData = subscriptionServiceRelatedData,
            payerVpa = Nothing,
            lastPaymentLinkSentAtIstDate = Just now,
            vehicleCategory = Just plan.vehicleCategory,
            isOnFreeTrial = True,
            isCategoryLevelSubscriptionEnabled = Nothing,
            lastBillGeneratedAt = Nothing,
            totalAmountChargedForService = 0,
            ..
          }
  QDPlan.create dPlan

getSubsriptionConfigAndPlanGeneric ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  ServiceNames ->
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe DriverPlan ->
  m (SubscriptionConfig, [Plan])
getSubsriptionConfigAndPlanGeneric serviceName (_, _, merchantOpCityId) mbDPlan = do
  subscriptionConfig <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId serviceName >>= fromMaybeM (NoSubscriptionConfigForService merchantOpCityId.getId $ show serviceName)
  plans <- QPD.findByMerchantOpCityIdAndPaymentModeWithServiceName merchantOpCityId (maybe AUTOPAY (.planType) mbDPlan) serviceName (Just False)
  return (subscriptionConfig, plans)

getSubsriptionConfigAndPlanSubscription ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  ServiceNames ->
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe DriverPlan ->
  m (SubscriptionConfig, [Plan])
getSubsriptionConfigAndPlanSubscription serviceName (driverId, _, merchantOpCityId) mbDPlan = do
  subscriptionConfig <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId serviceName >>= fromMaybeM (NoSubscriptionConfigForService merchantOpCityId.getId $ show serviceName)
  vehicleCategory <- getVehicleCategory driverId subscriptionConfig
  let planModeToList = maybe AUTOPAY (.planType) mbDPlan
  plans <- QPD.findByMerchantOpCityIdTypeServiceNameVehicle merchantOpCityId planModeToList serviceName vehicleCategory False
  return (subscriptionConfig, plans)

-- This API is for listing all the AUTO PAY plans
planList ::
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ServiceNames ->
  Maybe Int ->
  Maybe Int ->
  Maybe Vehicle.VehicleVariant ->
  Flow PlanListAPIRes
planList (driverId, merchantId, merchantOpCityId) serviceName _mbLimit _mbOffset _mbVariant = do
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  mDriverPlan <- B.runInReplica $ QDPlan.findByDriverIdWithServiceName driverId serviceName
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  (_, plans) <- getSubscriptionConfigAndPlan serviceName (driverId, merchantId, merchantOpCityId) mDriverPlan
  now <- getCurrentTime
  let mandateSetupDate = fromMaybe now ((.mandateSetupDate) =<< mDriverPlan)
  plansList <-
    mapM
      ( \plan' ->
          if driverInfo.autoPayStatus == Just DI.ACTIVE
            then do convertPlanToPlanEntity driverId mandateSetupDate False plan'
            else do convertPlanToPlanEntity driverId now False plan'
      )
      $ sortOn (.listingPriority) plans
  return $
    PlanListAPIRes
      { list = plansList,
        subscriptionStartTime = transporterConfig.subscriptionStartTime,
        isLocalized = Just True
      }

-- This API is for listing current driver plan
currentPlan ::
  ServiceNames ->
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Flow CurrentPlanRes
currentPlan serviceName (driverId, _merchantId, merchantOperatingCityId) = do
  driverInfo <- DI.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
  (autoPayStatus, mDriverPlan) <- getSubcriptionStatusWithPlan serviceName driverId
  mPlan <- maybe (pure Nothing) (\p -> QPD.findByIdAndPaymentModeWithServiceName p.planId (maybe AUTOPAY (.planType) mDriverPlan) serviceName) mDriverPlan
  mandateDetailsEntity <- mkMandateDetailEntity (join (mDriverPlan <&> (.mandateId)))
  let isEligibleForCharge = mDriverPlan <&> (.enableServiceUsageCharge)
  latestManualPayment <- QDF.findLatestByFeeTypeAndStatusWithServiceName DF.RECURRING_INVOICE [DF.CLEARED, DF.COLLECTED_CASH] driverId serviceName
  latestAutopayPayment <- QDF.findLatestByFeeTypeAndStatusWithServiceName DF.RECURRING_EXECUTION_INVOICE [DF.CLEARED] driverId serviceName
  vehicleCategory <- QVehicle.findById driverId
  now <- getCurrentTime
  let mbMandateSetupDate = mDriverPlan >>= (.mandateSetupDate)
  let mandateSetupDate = maybe now (\date -> if checkIFActiveStatus autoPayStatus then date else now) mbMandateSetupDate
  currentPlanEntity <- maybe (pure Nothing) (convertPlanToPlanEntity driverId mandateSetupDate True >=> (pure . Just)) mPlan
  mbInvoice <- listToMaybe <$> QINV.findLatestNonAutopayActiveByDriverId driverId serviceName
  (orderId, lastPaymentType) <-
    case mbInvoice of
      Just invoice -> do
        mbOrder <- if invoice.invoiceStatus == INV.ACTIVE_INVOICE then SOrder.findById (cast invoice.id) else return Nothing
        maybe (pure (Nothing, Nothing)) orderBasedCheck mbOrder
      Nothing -> return (Nothing, Nothing)
  let askForPlanSwitchByCity = maybe False (merchantOperatingCityId /=) (mDriverPlan <&> (.merchantOpCityId))
  let askForPlanSwitchByVehicle = (vehicleCategory >>= (.category)) /= (mDriverPlan >>= (.vehicleCategory)) && isJust mDriverPlan
  return $
    CurrentPlanRes
      { currentPlanDetails = currentPlanEntity,
        mandateDetails = mandateDetailsEntity,
        autoPayStatus = autoPayStatus,
        subscribed = driverInfo.subscribed,
        orderId,
        lastPaymentType,
        latestManualPaymentDate = latestManualPayment <&> (.updatedAt),
        latestAutopayPaymentDate = latestAutopayPayment <&> (.updatedAt),
        planRegistrationDate = mDriverPlan <&> (.createdAt),
        isLocalized = Just True,
        isEligibleForCharge,
        payoutVpa = driverInfo.payoutVpa,
        ..
      }
  where
    checkIFActiveStatus (Just DI.ACTIVE) = True
    checkIFActiveStatus _ = False
    orderBasedCheck order = do
      if order.status `elem` [Payment.NEW, Payment.PENDING_VBV, Payment.AUTHORIZING, Payment.STARTED]
        then return (Just order.id, if isJust order.createMandate then Just AUTOPAY_REGISTRATION else Just CLEAR_DUE)
        else return (Nothing, Nothing)

-- This API is to create a mandate order if the driver has not subscribed to Mandate even once or has Cancelled Mandate from PSP App.
planSubscribeGeneric ::
  ServiceNames ->
  Id Plan ->
  (Bool, Maybe MessageKey.MediaChannel) ->
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DI.DriverInformation ->
  SubscriptionServiceRelatedData ->
  Flow PlanSubscribeRes
planSubscribeGeneric serviceName planId (isDashboard, channel) (driverId, merchantId, merchantOpCityId) _ subscriptionServiceRelatedData = do
  (autoPayStatus, driverPlan) <- getSubcriptionStatusWithPlan serviceName driverId
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId serviceName
      >>= fromMaybeM (NoSubscriptionConfigForService merchantOpCityId.getId $ show serviceName)
  let deepLinkExpiry = subscriptionConfig.deepLinkExpiryTimeInMinutes
  let allowDeepLink = subscriptionConfig.sendDeepLink
  let mbDeepLinkData = if isDashboard && allowDeepLink then Just $ SPayment.DeepLinkData {sendDeepLink = Just True, expiryTimeInMinutes = deepLinkExpiry} else Nothing
      paymentServiceName = subscriptionConfig.paymentServiceName
  when (autoPayStatus == Just DI.ACTIVE) $ throwError InvalidAutoPayStatus
  plan <- QPD.findByIdAndPaymentModeWithServiceName planId MANUAL serviceName >>= fromMaybeM (PlanNotFound planId.getId)
  let isSamePlan = maybe False (\dp -> dp.planId == planId) driverPlan
  let isSubscriptionEnabledForCity = maybe False (\vcList -> isJust $ find (plan.vehicleCategory ==) vcList) subscriptionConfig.subscriptionEnabledForVehicleCategories
  unless isSubscriptionEnabledForCity $ throwError (InternalError $ "Subscription is not enabled for city :- " <> merchantOpCityId.getId <> " and vehicle category :- " <> (show plan.vehicleCategory))
  when (autoPayStatus == Just DI.PAUSED_PSP) $ do
    let mbMandateId = (.mandateId) =<< driverPlan
    whenJust mbMandateId $ \mandateId -> do
      fork "Cancelling paused Mandate" $ do
        void $ Payment.mandateRevoke merchantId merchantOpCityId paymentServiceName (Payment.MandateRevokeReq {mandateId = mandateId.getId})
  unless (autoPayStatus == Just DI.PENDING) $ do
    updateSubscriptionStatus serviceName (driverId, merchantId, merchantOpCityId) (Just DI.PENDING) Nothing
  when (isNothing driverPlan) $ do
    createDriverPlan serviceName (driverId, merchantId, merchantOpCityId) plan subscriptionServiceRelatedData
  when (isJust driverPlan) $ do
    unless (autoPayStatus == Just DI.PENDING && isSamePlan) $ do
      QDF.updateRegisterationFeeStatusByDriverIdForServiceName DF.INACTIVE driverId serviceName
    QDPlan.updatePlanIdByDriverIdAndServiceName driverId planId serviceName (Just plan.vehicleCategory) plan.merchantOpCityId
  (createOrderResp, orderId) <- createMandateInvoiceAndOrder serviceName driverId merchantId merchantOpCityId plan mbDeepLinkData
  when isDashboard $ do
    fork "send link through dashboard" $ do
      SPayment.sendLinkTroughChannelProvided createOrderResp.payment_links driverId Nothing channel allowDeepLink MessageKey.WHATSAPP_SETUP_MANDATE_MESSAGE
  return $
    PlanSubscribeRes
      { orderId = orderId,
        orderResp = createOrderResp
      }

mkDriverPlan :: (MonadFlow m) => Plan -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> m DriverPlan
mkDriverPlan plan (driverId, merchantId, merchantOpCityId) = do
  now <- getCurrentTime
  return $
    DriverPlan
      { driverId = cast driverId,
        planId = plan.id,
        planType = plan.paymentMode,
        mandateId = Nothing,
        createdAt = now,
        updatedAt = now,
        mandateSetupDate = Nothing,
        coinCovertedToCashLeft = 0.0,
        totalCoinsConvertedCash = 0.0,
        enableServiceUsageCharge = True,
        payerVpa = Nothing,
        serviceName = plan.serviceName,
        autoPayStatus = Just DI.PENDING,
        merchantOpCityId = merchantOpCityId,
        subscriptionServiceRelatedData = NoData,
        lastPaymentLinkSentAtIstDate = Just now,
        vehicleCategory = Just plan.vehicleCategory,
        isOnFreeTrial = True,
        isCategoryLevelSubscriptionEnabled = Nothing,
        lastBillGeneratedAt = Nothing,
        totalAmountChargedForService = 0,
        ..
      }

-- This API is to switch between plans of current Payment Method Preference.
planSwitchGeneric :: ServiceNames -> Id Plan -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow APISuccess
planSwitchGeneric serviceName planId (driverId, _, merchantOpCityId) = do
  void $ B.runInReplica $ QDPlan.findByDriverIdWithServiceName driverId serviceName >>= fromMaybeM (NoCurrentPlanForDriver driverId.getId)
  (autoPayStatus, _) <- getSubcriptionStatusWithPlan serviceName driverId
  plan <- QPD.findByIdAndPaymentModeWithServiceName planId (getDriverPaymentMode autoPayStatus) serviceName >>= fromMaybeM (PlanNotFound planId.getId)
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId serviceName
  let isSubscriptionEnabledAtCategoryLevel = fromMaybe False (subscriptionConfig <&> (.enableCityBasedFeeSwitch))
  QDPlan.updatePlanIdByDriverIdAndServiceName driverId planId serviceName (Just plan.vehicleCategory) plan.merchantOpCityId
  when (serviceName == YATRI_SUBSCRIPTION) $ do
    driverManualDuesFees <- QDF.findAllByStatusAndDriverIdWithServiceName driverId [DF.PAYMENT_OVERDUE] Nothing serviceName
    let currentDues = calculateDues driverManualDuesFees
    when plan.subscribedFlagToggleAllowed $ DI.updateSubscription (currentDues < plan.maxCreditLimit) driverId
  (from, to) <- getStartTimeAndEndTimeRange merchantOpCityId driverId Nothing
  fork "update driver fee in plan switch" $ do
    QDF.updateDfeeByOperatingCityAndVehicleCategory merchantOpCityId driverId serviceName DF.ONGOING from to plan.vehicleCategory plan.id.getId isSubscriptionEnabledAtCategoryLevel
  return Success
  where
    getDriverPaymentMode = \case
      Just DI.ACTIVE -> AUTOPAY
      Just DI.SUSPENDED -> MANUAL
      Just DI.PAUSED_PSP -> MANUAL
      Just DI.CANCELLED_PSP -> MANUAL
      _ -> MANUAL
    calculateDues driverFees = sum $ map (\dueInvoice -> roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) driverFees

-- This API is to make Mandate Inactive and switch to Manual plan type from Autopay.
planSuspendGeneric :: ServiceNames -> Bool -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow APISuccess
planSuspendGeneric serviceName isDashboard (driverId, _merchantId, merchantOpCityId) = do
  driver <- B.runInReplica $ QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  autoPayStatusAndDplan <- getSubcriptionStatusWithPlan serviceName driverId
  case autoPayStatusAndDplan of
    (Just DI.ACTIVE, Just driverPlan) -> do
      mandate <- validateActiveMandateExists driverId driverPlan
      Redis.whenWithLockRedis (DF.mandateProcessingLockKey mandate.id.getId) 60 $ do
        QM.updateStatus DM.INACTIVE mandate.id
        QDPlan.updatePaymentModeByDriverIdAndServiceName MANUAL (cast driverPlan.driverId) serviceName
        updateSubscriptionStatus serviceName (driverId, _merchantId, merchantOpCityId) (Just DI.SUSPENDED) Nothing
        QDF.updateAllExecutionPendingToManualOverdueByDriverIdForServiceName (cast driverId) serviceName
        QINV.inActivateAllAutopayActiveInvoices (cast driverId) serviceName
      when isDashboard $ notifyPaymentModeManualOnSuspend merchantOpCityId driver
    (Just _, Just _) -> throwError InvalidAutoPayStatus
    (_, _) -> throwError $ NoCurrentPlanForDriver driverId.getId
  return Success

-- This API is to make Mandate Active and switch to Autopay plan type. If an only if an Auto Pay plan was paused/cancelled by driver from App.
planResumeGeneric :: ServiceNames -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow APISuccess
planResumeGeneric serviceName (driverId, _merchantId, _merchantOpCityId) = do
  now <- getCurrentTime
  autoPayStatusAndDplan <- getSubcriptionStatusWithPlan serviceName driverId
  case autoPayStatusAndDplan of
    (Just DI.SUSPENDED, Just driverPlan) -> do
      mandate <- validateInActiveMandateExists driverId driverPlan
      Redis.whenWithLockRedis (DF.mandateProcessingLockKey mandate.id.getId) 60 $ do
        QM.updateStatus DM.ACTIVE mandate.id
        QDPlan.updateMandateSetupDateByDriverIdAndServiceName (Just now) (cast driverPlan.driverId) serviceName
        QDPlan.updatePaymentModeByDriverIdAndServiceName AUTOPAY (cast driverPlan.driverId) serviceName
        updateSubscriptionStatus serviceName (driverId, _merchantId, _merchantOpCityId) (Just DI.ACTIVE) Nothing
    (Just _, Just _) -> throwError InvalidAutoPayStatus
    (_, _) -> throwError $ NoCurrentPlanForDriver driverId.getId
  return Success

---------------------------------------------------------------------------------------------------------
------------------------------------------ Helper Functions ---------------------------------------------
---------------------------------------------------------------------------------------------------------

validateActiveMandateExists :: Id SP.Person -> DriverPlan -> Flow DM.Mandate
validateActiveMandateExists driverId driverPlan = do
  case driverPlan.mandateId of
    Nothing -> throwError $ ActiveMandateDoNotExist driverId.getId
    Just mandateId -> do
      mandate <- B.runInReplica $ QM.findById mandateId >>= fromMaybeM (MandateNotFound mandateId.getId)
      unless (mandate.status == DM.ACTIVE) $ throwError (ActiveMandateDoNotExist driverId.getId)
      return mandate

validateInActiveMandateExists :: Id SP.Person -> DriverPlan -> Flow DM.Mandate
validateInActiveMandateExists driverId driverPlan = do
  case driverPlan.mandateId of
    Nothing -> throwError $ InActiveMandateDoNotExist driverId.getId
    Just mandateId -> do
      mandate <- QM.findById mandateId >>= fromMaybeM (MandateNotFound mandateId.getId)
      unless (mandate.status == DM.INACTIVE) $ throwError (InActiveMandateDoNotExist driverId.getId)
      return mandate

createMandateInvoiceAndOrder ::
  ServiceNames ->
  Id SP.Person ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Plan ->
  Maybe SPayment.DeepLinkData ->
  Flow (Payment.CreateOrderResp, Id DOrder.PaymentOrder)
createMandateInvoiceAndOrder serviceName driverId merchantId merchantOpCityId plan mbDeepLinkData = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName merchantOpCityId serviceName
      >>= fromMaybeM (NoSubscriptionConfigForService merchantOpCityId.getId $ show serviceName)
  let allowDueAddition = subscriptionConfig.allowDueAddition
  let paymentServiceName = subscriptionConfig.paymentServiceName
  driverManualDuesFees <- if allowDueAddition then QDF.findAllByStatusAndDriverIdWithServiceName driverId [DF.PAYMENT_OVERDUE] Nothing serviceName else return []
  let currentDues = calculateDues driverManualDuesFees
  now <- getCurrentTime
  (driverRegisterationFee, invoice) <- getLatestMandateRegistrationFeeAndCheckIfEligible currentDues now
  let maxMandateAmount = max plan.maxMandateAmount currentDues
  case (driverRegisterationFee, invoice) of
    (Just registerFee, Just inv) -> do
      isReusableInvoice <- checkIfInvoiceIsReusable inv (registerFee : driverManualDuesFees)
      if inv.maxMandateAmount == Just maxMandateAmount && isReusableInvoice
        then do
          let invoiceReuseForOrderCreation = Just (inv.id, inv.invoiceShortId)
          createOrderForDriverFee driverManualDuesFees registerFee currentDues now transporterConfig.mandateValidity invoiceReuseForOrderCreation paymentServiceName subscriptionConfig
        else do
          QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE inv.id
          createOrderForDriverFee driverManualDuesFees registerFee currentDues now transporterConfig.mandateValidity Nothing paymentServiceName subscriptionConfig
    (Just registerFee, Nothing) -> do
      createOrderForDriverFee driverManualDuesFees registerFee currentDues now transporterConfig.mandateValidity Nothing paymentServiceName subscriptionConfig
    (Nothing, _) -> do
      driverFee <- mkDriverFee currentDues currency
      QDF.create driverFee
      createOrderForDriverFee driverManualDuesFees driverFee currentDues now transporterConfig.mandateValidity Nothing paymentServiceName subscriptionConfig
  where
    mandateOrder currentDues now mandateValidity =
      SPayment.MandateOrder
        { maxAmount = max plan.maxMandateAmount currentDues,
          _type = Payment.REQUIRED,
          frequency = Payment.ASPRESENTED,
          mandateStartDate = T.pack $ show $ utcTimeToPOSIXSeconds now,
          mandateEndDate = T.pack $ show $ utcTimeToPOSIXSeconds $ addUTCTime (secondsToNominalDiffTime (fromIntegral (60 * 60 * 24 * 365 * mandateValidity))) now
        }
    getLatestMandateRegistrationFeeAndCheckIfEligible currentDues' now = do
      registerFee' <- QDF.findLatestRegisterationFeeByDriverIdAndServiceName (cast driverId) serviceName
      case registerFee' of
        Just registerFee -> do
          invoices <- QINV.findActiveMandateSetupInvoiceByFeeId registerFee.id Domain.MANDATE_SETUP_INVOICE Domain.ACTIVE_INVOICE
          mbInvoiceToReuse <- do
            case invoices of
              [] -> pure Nothing
              (inv : resActiveInvoices) -> do
                -- ideally resActiveInvoices should be null in case they are there make them inactive
                mapM_ (QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE . (.id)) resActiveInvoices
                pure $ Just inv
          let totalRegisFee = registerFee.platformFee.fee + registerFee.platformFee.cgst + registerFee.platformFee.sgst
          case (totalRegisFee > 0, currentDues' > 0) of
            (True, True) -> do
              QDF.updateStatus DF.INACTIVE registerFee.id now
              case mbInvoiceToReuse of
                Just invoiceToReuse -> do
                  QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE invoiceToReuse.id
                Nothing -> pure ()
              return (Nothing, Nothing)
            (False, False) -> do
              QDF.updateStatus DF.CLEARED registerFee.id now
              case mbInvoiceToReuse of
                Just invoiceToReuse -> do
                  QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE invoiceToReuse.id
                Nothing -> pure ()
              return (Nothing, Nothing)
            _ -> return (registerFee', mbInvoiceToReuse)
        Nothing -> return (Nothing, Nothing)
    createOrderForDriverFee driverManualDuesFees driverFee currentDues now mandateValidity mbInvoiceIdTuple paymentServiceName subscriptionConfig = do
      let mbMandateOrder = Just $ mandateOrder currentDues now mandateValidity
          splitEnabled = subscriptionConfig.isVendorSplitEnabled == Just True
      if not (null driverManualDuesFees)
        then do
          vendorFees' <- if splitEnabled then concat <$> mapM (QVF.findAllByDriverFeeId . DF.id) driverManualDuesFees else pure []
          let vendorFees = map roundVendorFee vendorFees'
          SPayment.createOrder (driverId, merchantId, merchantOpCityId) paymentServiceName (driverFee : driverManualDuesFees, []) mbMandateOrder INV.MANDATE_SETUP_INVOICE mbInvoiceIdTuple vendorFees mbDeepLinkData splitEnabled
        else do
          SPayment.createOrder (driverId, merchantId, merchantOpCityId) paymentServiceName ([driverFee], []) mbMandateOrder INV.MANDATE_SETUP_INVOICE mbInvoiceIdTuple [] mbDeepLinkData splitEnabled
    mkDriverFee currentDues currency = do
      let (fee, cgst, sgst) = if currentDues > 0 then (0.0, 0.0, 0.0) else calculatePlatformFeeAttr plan.registrationAmount plan
      id <- generateGUID
      now <- getCurrentTime
      return $
        DF.DriverFee
          { id = id,
            merchantId = merchantId,
            payBy = now,
            status = DF.PAYMENT_PENDING,
            numRides = 0,
            createdAt = now,
            updatedAt = now,
            platformFee = DF.PlatformFee {fee, cgst, sgst, currency},
            totalEarnings = 0,
            feeType = DF.MANDATE_REGISTRATION,
            govtCharges = 0,
            startTime = now,
            endTime = now,
            collectedBy = Nothing,
            driverId = cast driverId,
            offerId = Nothing,
            planOfferTitle = Nothing,
            autopayPaymentStage = Nothing,
            stageUpdatedAt = Nothing,
            billNumber = Nothing,
            feeWithoutDiscount = Nothing,
            schedulerTryCount = 0,
            collectedAt = Nothing,
            overlaySent = False,
            amountPaidByCoin = Nothing,
            specialZoneRideCount = 0,
            specialZoneAmount = 0,
            planId = Just $ plan.id,
            planMode = Just plan.paymentMode,
            notificationRetryCount = 0,
            badDebtDeclarationDate = Nothing,
            vehicleNumber = Nothing,
            badDebtRecoveryDate = Nothing,
            merchantOperatingCityId = merchantOpCityId,
            serviceName,
            currency,
            refundEntityId = Nothing,
            refundedAmount = Nothing,
            refundedAt = Nothing,
            refundedBy = Nothing,
            hasSibling = Just False,
            siblingFeeId = Nothing,
            splitOfDriverFeeId = Nothing,
            vehicleCategory = plan.vehicleCategory
          }
    calculateDues driverFees = sum $ map (\dueInvoice -> roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) driverFees
    checkIfInvoiceIsReusable invoice newDriverFees = do
      allDriverFeeClubedToInvoice <- QINV.findById invoice.id
      let oldLinkedDriverFeeIds = allDriverFeeClubedToInvoice <&> (.driverFeeId)
      let newDriverFeeIds = newDriverFees <&> (.id)
      let intersectionOfDriverFeeIds = oldLinkedDriverFeeIds `intersect` newDriverFeeIds
      return $ length oldLinkedDriverFeeIds == length intersectionOfDriverFeeIds && length newDriverFeeIds == length intersectionOfDriverFeeIds

convertPlanToPlanEntity :: Id SP.Person -> UTCTime -> Bool -> Plan -> Flow PlanEntity
convertPlanToPlanEntity driverId applicationDate isCurrentPlanEntity plan@Plan {..} = do
  dueDriverFees <- B.runInReplica $ QDF.findAllPendingAndDueDriverFeeByDriverIdForServiceName driverId serviceName
  pendingRegistrationDfee <- B.runInReplica $ QDF.findAllPendingRegistrationDriverFeeByDriverIdForServiceName driverId serviceName
  transporterConfig_ <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
  paymentCurrency <- case currency of
    INR -> pure INR
    _ -> throwError $ InvalidRequest "Invalid currency" -- is it correct?
  offers <- SPayment.offerListCache merchantId merchantOpCityId plan.serviceName =<< makeOfferReq paymentCurrency applicationDate plan.paymentMode transporterConfig_
  let allPendingAndOverDueDriverfee = dueDriverFees <> pendingRegistrationDfee
  invoicesForDfee <- QINV.findByDriverFeeIds (map (.id) allPendingAndOverDueDriverfee)
  now <- getCurrentTime
  mbDriverStat <- QDS.findById (cast driverId)
  let planFareBreakup = mkPlanFareBreakup currency offers.offerResp
      coinCashLeft = if plan.eligibleForCoinDiscount then Just $ max 0.0 $ maybe 0.0 (.coinCovertedToCashLeft) mbDriverStat else Nothing
      planChargeWithDiscount = find (\x -> x.component == "DISCOUNTED_FEE") planFareBreakup <&> (.amount)
      maxPlanCharge = find (\x -> x.component == "MAX_FEE_LIMIT") planFareBreakup <&> (.amount)
      coinDiscountUpto = min <$> coinCashLeft <*> minMaybe planChargeWithDiscount maxPlanCharge
  driver <- B.runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mbtranslation <- CQPTD.findByPlanIdAndLanguage plan.id (fromMaybe ENGLISH driver.language)
  let translatedName = maybe plan.name (.name) mbtranslation
      translatedDescription = maybe plan.description (.description) mbtranslation
      planBaseFrequcency = getPlanBaseFrequency planBaseAmount

  dues <-
    if isCurrentPlanEntity
      then do mkDueDriverFeeInfoEntity serviceName dueDriverFees transporterConfig_
      else return []
  let currentDues = sum $ map (.driverFeeAmount) dues
  let autopayDues = sum $ map (.driverFeeAmount) $ filter (\due -> due.feeType == DF.RECURRING_EXECUTION_INVOICE) dues
  let dueBoothCharges = roundToHalf currency $ sum $ map (.specialZoneAmount) (nubBy (\x y -> (x.startTime == y.startTime) && (x.endTime == y.endTime)) dueDriverFees)

  return
    PlanEntity
      { id = plan.id.getId,
        offers = makeOfferEntity <$> offers.offerResp,
        frequency = planBaseFrequcency,
        name = translatedName,
        description = translatedDescription,
        currentDues,
        autopayDues,
        totalPlanCreditLimit = round maxCreditLimit,
        bankErrors = if isCurrentPlanEntity then calcBankError currency allPendingAndOverDueDriverfee transporterConfig_ now invoicesForDfee else [],
        totalPlanCreditLimitWithCurrency = PriceAPIEntity maxCreditLimit currency,
        currentDuesWithCurrency = PriceAPIEntity currentDues currency,
        autopayDuesWithCurrency = PriceAPIEntity autopayDues currency,
        dueBoothChargesWithCurrency = PriceAPIEntity dueBoothCharges currency,
        coinEntity = CoinEntity <$> coinDiscountUpto <*> (PriceAPIEntity <$> coinDiscountUpto <*> pure currency),
        ..
      }
  where
    makeOfferEntity offer =
      OfferEntity
        { title = offer.offerDescription.title,
          description = offer.offerDescription.description,
          tnc = offer.offerDescription.tnc,
          offerId = offer.offerId
        }
    makeOfferReq paymentCurrency date paymentMode_ transporterConfig = do
      let baseAmount = case plan.planBaseAmount of
            PERRIDE_BASE amount -> amount
            DAILY_BASE amount -> amount
            WEEKLY_BASE amount -> amount
            MONTHLY_BASE amount -> amount
      driver <- QP.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
      now <- getCurrentTime

      let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = baseAmount, currency = paymentCurrency}
          customerReq = Payment.OfferCustomer {customerId = driverId.getId, email = driver.email, mobile = Nothing}
      return
        Payment.OfferListReq
          { order = offerOrder,
            customer = Just customerReq,
            planId = plan.id.getId,
            registrationDate = addUTCTime (fromIntegral transporterConfig.timeDiffFromUtc) date,
            dutyDate = addUTCTime (fromIntegral transporterConfig.timeDiffFromUtc) now,
            paymentMode = getPaymentModeAndVehicleCategoryKey plan,
            numOfRides = if paymentMode_ == AUTOPAY then 0 else -1,
            offerListingMetric = if transporterConfig.enableUdfForOffers then Just Payment.IS_VISIBLE else Nothing
          }
    mkPlanFareBreakup currency offers = do
      let baseAmount = case plan.planBaseAmount of
            PERRIDE_BASE amount -> amount
            DAILY_BASE amount -> amount
            WEEKLY_BASE amount -> amount
            MONTHLY_BASE amount -> amount
          (discountAmount, finalOrderAmount) =
            if null offers
              then (0.0, baseAmount)
              else do
                let bestOffer = DL.minimumBy (comparing (.finalOrderAmount)) offers
                if plan.allowStrikeOff
                  then (bestOffer.discountAmount, bestOffer.finalOrderAmount)
                  else (0.0, baseAmount)
      [ PlanFareBreakup {component = "INITIAL_BASE_FEE", amount = baseAmount, amountWithCurrency = PriceAPIEntity baseAmount currency},
        PlanFareBreakup {component = "REGISTRATION_FEE", amount = plan.registrationAmount, amountWithCurrency = PriceAPIEntity plan.registrationAmount currency},
        PlanFareBreakup {component = "MAX_FEE_LIMIT", amount = plan.maxAmount, amountWithCurrency = PriceAPIEntity plan.maxAmount currency},
        PlanFareBreakup {component = "DISCOUNTED_FEE", amount = discountAmount, amountWithCurrency = PriceAPIEntity discountAmount currency},
        PlanFareBreakup {component = "FINAL_FEE", amount = finalOrderAmount, amountWithCurrency = PriceAPIEntity finalOrderAmount currency}
        ]
    driverFeeAndInvoiceIdsWithValidError currency transporterConfig mapDfee now =
      mapMaybe
        ( \invoice -> do
            let isExpiredError = now > maybe now (addUTCTime transporterConfig.bankErrorExpiry) invoice.bankErrorUpdatedAt
            case (invoice.bankErrorMessage, invoice.bankErrorCode, mapDfee M.!? invoice.driverFeeId, isExpiredError) of
              (Just message, Just code, Just dfee, False) ->
                do Just
                  ErrorEntity
                    { message = message,
                      code = code,
                      amount = dfee.govtCharges + dfee.platformFee.fee + dfee.platformFee.cgst + dfee.platformFee.sgst,
                      amountWithCurrency = PriceAPIEntity (dfee.govtCharges + dfee.platformFee.fee + dfee.platformFee.cgst + dfee.platformFee.sgst) currency
                    }
              (_, _, _, _) -> Nothing
        )
    getLatestInvoice = map (maximumBy (compare `on` INV.createdAt)) . groupBy (\a b -> a.driverFeeId == b.driverFeeId) . sortBy (compare `on` INV.driverFeeId)
    calcBankError currency allPendingAndOverDueDriverfee transporterConfig_ now invoicesForDfee = do
      let mapDriverFeeByDriverFeeId = M.fromList (map (\df -> (df.id, df)) allPendingAndOverDueDriverfee)
      driverFeeAndInvoiceIdsWithValidError currency transporterConfig_ mapDriverFeeByDriverFeeId now (getLatestInvoice invoicesForDfee)

getPlanBaseFrequency :: PlanBaseAmount -> Text
getPlanBaseFrequency planBaseAmount = case planBaseAmount of
  PERRIDE_BASE _ -> "PER_RIDE"
  DAILY_BASE _ -> "DAILY"
  WEEKLY_BASE _ -> "WEEKLY"
  MONTHLY_BASE _ -> "MONTHLY"

mkMandateDetailEntity :: Maybe (Id DM.Mandate) -> Flow (Maybe MandateDetailsEntity)
mkMandateDetailEntity mandateId = do
  case mandateId of
    Just id -> do
      mandate <- B.runInReplica $ QM.findById id >>= fromMaybeM (MandateNotFound id.getId)
      return $
        Just
          MandateDetails
            { status = mandate.status,
              startDate = mandate.startDate,
              endDate = mandate.endDate,
              mandateId = mandate.id.getId,
              payerVpa = mandate.payerVpa,
              frequency = "Aspresented",
              maxAmount = round mandate.maxAmount,
              maxAmountWithCurrency = PriceAPIEntity mandate.maxAmount mandate.currency,
              payerApp = mandate.payerApp,
              autopaySetupDate = mandate.createdAt
            }
    Nothing -> return Nothing

mkDueDriverFeeInfoEntity ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  ServiceNames ->
  [DF.DriverFee] ->
  TransporterConfig ->
  m [DriverDuesEntity]
mkDueDriverFeeInfoEntity serviceName driverFees transporterConfig = do
  mapM
    ( \driverFee -> do
        driverFeesInWindow <- QDF.findFeeInRangeAndDriverIdAndServiceName driverFee.startTime driverFee.endTime driverFee.driverId serviceName
        invoice <- listToMaybe <$> QINV.findActiveByDriverFeeIds [driverFee.id]
        mbPlan <- getPlanDataFromDriverFee driverFee
        let invoiceType = invoice <&> (.paymentMode)
            maxRidesEligibleForCharge = planMaxRides =<< mbPlan
            createdAt =
              if invoiceType `elem` [Just INV.MANUAL_INVOICE, Just INV.MANDATE_SETUP_INVOICE, Nothing]
                then invoice <&> (.createdAt)
                else Nothing
            executionAt =
              if invoiceType == Just INV.AUTOPAY_INVOICE
                then calcExecutionTime transporterConfig (driverFee.autopayPaymentStage) <$> (driverFee.stageUpdatedAt)
                else Nothing
            feeType
              | (\dfee -> dfee.feeType == DF.MANDATE_REGISTRATION) driverFee = DF.MANDATE_REGISTRATION
              | invoiceType == Just INV.AUTOPAY_INVOICE = DF.RECURRING_EXECUTION_INVOICE
              | otherwise = DF.RECURRING_INVOICE
        -- FIXME should we round to half?
        let driverFeeAmount = roundToHalf driverFee.currency (driverFee.govtCharges + driverFee.platformFee.fee + driverFee.platformFee.cgst + driverFee.platformFee.sgst)
        return
          DriverDuesEntity
            { autoPayStage = driverFee.autopayPaymentStage,
              paymentStatus = invoice <&> (.invoiceStatus),
              totalEarnings = driverFee.totalEarnings,
              totalRides = calcNumRides driverFee transporterConfig,
              planAmount = fromMaybe 0.0 driverFee.feeWithoutDiscount,
              planAmountWithCurrency = PriceAPIEntity (fromMaybe 0.0 driverFee.feeWithoutDiscount) driverFee.currency,
              isSplit = length driverFeesInWindow > 1,
              offerAndPlanDetails = driverFee.planOfferTitle,
              rideTakenOn = addUTCTime (-1 * secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) driverFee.createdAt, --- when we fix ist issue we will remove this
              driverFeeAmount,
              driverFeeAmountWithCurrency = PriceAPIEntity driverFeeAmount driverFee.currency,
              createdAt,
              executionAt,
              feeType,
              maxRidesEligibleForCharge,
              isCoinCleared = driverFee.status == DF.CLEARED_BY_YATRI_COINS,
              coinDiscountAmount = driverFee.amountPaidByCoin,
              coinDiscountAmountWithCurrency = flip PriceAPIEntity driverFee.currency <$> driverFee.amountPaidByCoin,
              specialZoneRideCount = driverFee.specialZoneRideCount,
              totalSpecialZoneCharges = driverFee.specialZoneAmount,
              totalSpecialZoneChargesWithCurrency = PriceAPIEntity driverFee.specialZoneAmount driverFee.currency,
              totalEarningsWithCurrency = PriceAPIEntity driverFee.totalEarnings driverFee.currency,
              driverFeeId = driverFee.id.getId,
              status = driverFee.status
            }
    )
    driverFees

planMaxRides :: Plan -> Maybe Int
planMaxRides plan = do
  case plan.planBaseAmount of
    PERRIDE_BASE baseAmount -> Just $ round $ (plan.maxAmount) / baseAmount
    _ -> Nothing

getPlanDataFromDriverFee ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DF.DriverFee ->
  m (Maybe Plan)
getPlanDataFromDriverFee driverFee = do
  let (mbPlanId, mbPaymentMode, serviceName) = (driverFee.planId, driverFee.planMode, driverFee.serviceName)
  case (mbPlanId, mbPaymentMode) of
    (Just planId, _) -> do
      let paymentMode = fromMaybe MANUAL mbPaymentMode
      QPD.findByIdAndPaymentModeWithServiceName planId paymentMode serviceName
    _ -> return Nothing

calcExecutionTime :: TransporterConfig -> Maybe DF.AutopayPaymentStage -> UTCTime -> UTCTime
calcExecutionTime transporterConfig' autopayPaymentStage scheduledAt = do
  let notificationTimeDiff = transporterConfig'.driverAutoPayNotificationTime
      executionTimeDiff = transporterConfig'.driverAutoPayExecutionTime
  case autopayPaymentStage of
    Just DF.NOTIFICATION_SCHEDULED -> addUTCTime (notificationTimeDiff + executionTimeDiff) scheduledAt
    Just DF.NOTIFICATION_ATTEMPTING -> addUTCTime executionTimeDiff scheduledAt
    Just DF.EXECUTION_SCHEDULED -> addUTCTime executionTimeDiff scheduledAt
    _ -> scheduledAt

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe (Just a) (Just b) = Just $ min a b
minMaybe a b = a <|> b

getVehicleCategory :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> SubscriptionConfig -> m DVC.VehicleCategory
getVehicleCategory personId subscriptionConfig = do
  mVehicle <- QVehicle.findById personId
  return $ fromMaybe subscriptionConfig.defaultCityVehicleCategory ((.category) =<< mVehicle)

isOnFreeTrial :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> SubscriptionConfig -> Int -> Maybe DriverPlan -> m (Bool, Maybe Int)
isOnFreeTrial driverId subscriptionConfig freeTrialDaysLeft mbDriverPlan = do
  numRides :: (Maybe Int) <- Redis.get $ mkCachedKeyTotalRidesByDriverId driverId
  case mbDriverPlan <&> (.isOnFreeTrial) of
    Just False -> return (False, Nothing)
    _ -> do
      driverStats <- if isJust numRides then pure Nothing else QDS.findByPrimaryKey driverId
      let freeTrialRides = fromMaybe 0 $ subscriptionConfig.numberOfFreeTrialRides
      let totalRides = fromMaybe 0 $ numRides <|> (driverStats <&> (.totalRides))
      let isOnFreeTrial' = not $ (totalRides > freeTrialRides && subscriptionConfig.freeTrialRidesApplicable) || freeTrialDaysLeft <= 0
      fork "update free trial flag" $ QDPlan.updateFreeTrialByDriverIdAndServiceName isOnFreeTrial' driverId subscriptionConfig.serviceName
      return (isOnFreeTrial', Just totalRides)
