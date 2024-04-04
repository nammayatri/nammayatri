{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Driver
  ( DriverInformationRes (..),
    GetHomeLocationsRes (..),
    AddHomeLocationReq (..),
    UpdateHomeLocationReq,
    DriverEntityRes (..),
    UpdateDriverReq (..),
    UpdateDriverRes,
    GetNearbySearchRequestsRes (..),
    DriverOfferReq (..),
    DriverRespondReq (..),
    DriverStatsRes (..),
    DriverAlternateNumberReq (..),
    DriverAlternateNumberRes (..),
    DriverAlternateNumberOtpReq (..),
    DriverPhotoUploadReq (..),
    ResendAuth (..),
    DriverPaymentHistoryResp,
    MetaDataReq (..),
    HistoryEntityV2 (..),
    HistoryEntryDetailsEntityV2 (..),
    ClearDuesRes (..),
    GetCityReq (..),
    GetCityResp (..),
    DriverFeeResp (..),
    getInformation,
    activateGoHomeFeature,
    deactivateGoHomeFeature,
    addHomeLocation,
    updateHomeLocation,
    getHomeLocations,
    deleteHomeLocation,
    setActivity,
    deleteDriver,
    updateDriver,
    getNearbySearchRequests,
    offerQuote,
    respondQuote,
    offerQuoteLockKey,
    getStats,
    driverPhotoUpload,
    validate,
    verifyAuth,
    resendOtp,
    remove,
    getDriverPayments,
    clearDriverDues,
    DriverInfo.DriverMode,
    updateMetaData,
    getDriverPaymentsHistoryV2,
    getHistoryEntryDetailsEntityV2,
    fetchDriverPhoto,
    getCity,
    getDownloadInvoiceData,
    getDummyRideRequest,
  )
where

import AWS.S3 as S3
import Control.Monad.Extra (mapMaybeM)
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified Data.Aeson as DA
import qualified Data.Aeson.KeyMap as DAKM
import Data.Digest.Pure.MD5 as MD5
import Data.Either.Extra (eitherToMaybe)
import Data.List (intersect, nub, (\\))
import qualified Data.List as DL
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Time (Day, diffDays, fromGregorian)
import Domain.Action.Dashboard.Driver.Notification as DriverNotify (triggerDummyRideRequest)
import Domain.Action.UI.DriverOnboarding.AadhaarVerification (fetchAndCacheAadhaarImage)
import qualified Domain.Action.UI.Person as SP
import qualified Domain.Action.UI.Plan as DAPlan
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Client as DC
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import qualified Domain.Types.Driver.GoHomeFeature.DriverHomeLocation as DDHL
import qualified Domain.Types.DriverFee as DDF
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.DriverQuote as DDrQuote
import qualified Domain.Types.DriverReferral as DR
import Domain.Types.DriverStats
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as Fare
import Domain.Types.FarePolicy (DriverExtraFeeBounds (..))
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Merchant.TransporterConfig
import Domain.Types.Person
import qualified Domain.Types.Person as SP
import Domain.Types.Plan as Plan
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchRequestForDriver as DSRD
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.ServiceTierType as DVST
import Domain.Types.Vehicle (VehicleAPIEntity)
import qualified Domain.Types.Vehicle as SV
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, state)
import qualified GHC.List as GHCL
import GHC.Records.Extra
import qualified IssueManagement.Domain.Types.MediaFile as Domain
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import Kernel.Beam.Functions
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import Kernel.External.Payment.Interface hiding (Currency)
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Verification.Interface.InternalScripts as IF
import Kernel.Prelude (NominalDiffTime, roundToIntegral)
import Kernel.Serviceability (rideServiceable)
import Kernel.Sms.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version
import Kernel.Utils.CalculateDistance
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Validation
import Kernel.Utils.Version
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Lib.Payment.Domain.Types.PaymentTransaction
import Lib.Payment.Storage.Queries.PaymentTransaction
import qualified Lib.Types.SpecialLocation as SL
import SharedLogic.Cac
import SharedLogic.CallBAP (sendDriverOffer, sendRideAssignedUpdateToBAP)
import qualified SharedLogic.DeleteDriver as DeleteDriverOnCheck
import qualified SharedLogic.DriverFee as SLDriverFee
import SharedLogic.DriverOnboarding
import SharedLogic.DriverPool as DP
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified SharedLogic.Merchant as SMerchant
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.Payment as SPayment
import SharedLogic.Ride
import qualified SharedLogic.SearchTryLocker as CS
import qualified Storage.Cac.DriverPoolConfig as SCDPC
import qualified Storage.Cac.GoHomeConfig as CGHC
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.BapMetadata as CQSM
import Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.FareProduct as CQFP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Driver.GoHomeFeature.DriverGoHomeRequest as QDGR
import qualified Storage.Queries.Driver.GoHomeFeature.DriverHomeLocation as QDHL
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverQuote as QDrQt
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as QFP
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.Invoice as QINV
import qualified Storage.Queries.MetaData as QMeta
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.RegistrationToken as QRegister
import Storage.Queries.Ride as Ride
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Storage.Queries.SearchTry as QST
import qualified Storage.Queries.Vehicle as QV
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Tools.Auth as Auth
import Tools.Error
import Tools.Event
import Tools.SMS as Sms hiding (Success)
import Tools.Verification
import Utils.Common.Cac.KeyNameConstants

data DriverInformationRes = DriverInformationRes
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    operatingCity :: Context.City,
    numberOfRides :: Int,
    mobileNumber :: Maybe Text,
    linkedVehicle :: Maybe VehicleAPIEntity,
    rating :: Maybe Centesimal,
    active :: Bool,
    onRide :: Bool,
    verified :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    subscribed :: Bool,
    paymentPending :: Bool,
    referralCode :: Maybe Text,
    organization :: DM.MerchantAPIEntity,
    language :: Maybe Maps.Language,
    alternateNumber :: Maybe Text,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    canDowngradeToTaxi :: Bool,
    canSwitchToRental :: Bool,
    canSwitchToInterCity :: Bool,
    mode :: Maybe DriverInfo.DriverMode,
    payerVpa :: Maybe Text,
    autoPayStatus :: Maybe DriverInfo.DriverAutoPayStatus,
    clientVersion :: Maybe Version,
    bundleVersion :: Maybe Version,
    gender :: Maybe SP.Gender,
    mediaUrl :: Maybe Text,
    aadhaarCardPhoto :: Maybe Text,
    isGoHomeEnabled :: Bool,
    driverGoHomeInfo :: DDGR.CachedGoHomeRequest,
    freeTrialDaysLeft :: Int,
    maskedDeviceToken :: Maybe Text,
    currentDues :: Maybe HighPrecMoney,
    manualDues :: Maybe HighPrecMoney,
    currentDuesWithCurrency :: Maybe PriceAPIEntity,
    manualDuesWithCurrency :: Maybe PriceAPIEntity,
    blockStateModifier :: Maybe Text,
    isVehicleSupported :: Bool,
    checkIfACWorking :: Bool,
    frontendConfigHash :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data DriverEntityRes = DriverEntityRes
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    linkedVehicle :: Maybe VehicleAPIEntity,
    rating :: Maybe Centesimal,
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    subscribed :: Bool,
    paymentPending :: Bool,
    verified :: Bool,
    registeredAt :: UTCTime,
    language :: Maybe Maps.Language,
    alternateNumber :: Maybe Text,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    canDowngradeToTaxi :: Bool,
    canSwitchToRental :: Bool,
    canSwitchToInterCity :: Bool,
    payerVpa :: Maybe Text,
    mode :: Maybe DriverInfo.DriverMode,
    autoPayStatus :: Maybe DriverInfo.DriverAutoPayStatus,
    clientVersion :: Maybe Version,
    bundleVersion :: Maybe Version,
    gender :: Maybe SP.Gender,
    mediaUrl :: Maybe Text,
    aadhaarCardPhoto :: Maybe Text,
    freeTrialDaysLeft :: Int,
    maskedDeviceToken :: Maybe Text,
    blockStateModifier :: Maybe Text,
    checkIfACWorking :: Bool,
    isVehicleSupported :: Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data UpdateDriverReq = UpdateDriverReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    deviceToken :: Maybe FCMRecipientToken,
    language :: Maybe Maps.Language,
    canDowngradeToSedan :: Maybe Bool,
    canDowngradeToHatchback :: Maybe Bool,
    canDowngradeToTaxi :: Maybe Bool,
    canSwitchToRental :: Maybe Bool,
    canSwitchToInterCity :: Maybe Bool,
    clientVersion :: Maybe Version,
    bundleVersion :: Maybe Version,
    gender :: Maybe SP.Gender,
    languagesSpoken :: Maybe [Text],
    hometown :: Maybe Text,
    vehicleName :: Maybe Text,
    availableUpiApps :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

validateUpdateDriverReq :: Validate UpdateDriverReq
validateUpdateDriverReq UpdateDriverReq {..} =
  sequenceA_
    [ validateField "firstName" firstName $ InMaybe $ MinLength 3 `And` P.name,
      validateField "middleName" middleName $ InMaybe $ NotEmpty `And` P.name,
      validateField "lastName" lastName $ InMaybe $ NotEmpty `And` P.name
    ]

type UpdateDriverRes = DriverInformationRes

newtype GetNearbySearchRequestsRes = GetNearbySearchRequestsRes
  { searchRequestsForDriver :: [USRD.SearchRequestForDriverAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverOfferReq = DriverOfferReq
  { offeredFare :: Maybe Money,
    offeredFareWithCurrency :: Maybe PriceAPIEntity,
    searchRequestId :: Id DST.SearchTry
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data DriverRespondReq = DriverRespondReq
  { offeredFare :: Maybe Money,
    offeredFareWithCurrency :: Maybe PriceAPIEntity,
    searchRequestId :: Maybe (Id DST.SearchTry), -- TODO: Deprecated, to be removed
    searchTryId :: Maybe (Id DST.SearchTry),
    response :: SearchRequestForDriverResponse
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data DriverStatsRes = DriverStatsRes
  { totalRidesOfDay :: Int,
    totalEarningsOfDay :: Money,
    totalEarningsOfDayPerKm :: Money,
    bonusEarning :: Money,
    totalEarningsOfDayWithCurrency :: PriceAPIEntity,
    totalEarningsOfDayPerKmWithCurrency :: PriceAPIEntity,
    bonusEarningWithCurrency :: PriceAPIEntity,
    coinBalance :: Int,
    totalValidRidesOfDay :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverPhotoUploadReq = DriverPhotoUploadReq
  { image :: Text,
    fileType :: S3.FileType,
    reqContentType :: Text,
    brisqueFeatures :: [Double]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverAlternateNumberReq = DriverAlternateNumberReq
  { mobileCountryCode :: Text,
    alternateNumber :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverAlternateNumberOtpReq = DriverAlternateNumberOtpReq
  { otp :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ResendAuth = ResendAuth
  { auth :: Text,
    attemptsLeft :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverAlternateNumberRes = DriverAlternateNumberRes
  { attempts :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverPaymentHistoryResp = DriverPaymentHistoryResp
  { date :: Day, -- window start day
    driverFeeId :: Id DDF.DriverFee,
    invoiceId :: Id INV.Invoice,
    status :: DDF.DriverFeeStatus,
    totalRides :: Int,
    totalEarnings :: Money,
    charges :: Money,
    totalEarningsWithCurrency :: PriceAPIEntity,
    chargesWithCurrency :: PriceAPIEntity,
    chargesBreakup :: [DriverPaymentBreakup],
    txnInfo :: [DriverTxnInfo]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverPaymentBreakup = DriverPaymentBreakup
  { component :: Text,
    amount :: HighPrecMoney,
    amountWithCurrency :: PriceAPIEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverTxnInfo = DriverTxnInfo
  { id :: Id PaymentTransaction,
    status :: TransactionStatus
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data AddHomeLocationReq = AddHomeLocationReq
  { position :: LatLong,
    address :: Text,
    tag :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

type UpdateHomeLocationReq = AddHomeLocationReq

newtype GetHomeLocationsRes = GetHomeLocationsRes
  { locations :: [DDHL.DriverHomeLocationAPIEntity]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data MetaDataReq = MetaDataReq
  { device :: Maybe Text,
    deviceOS :: Maybe Text,
    deviceDateTime :: Maybe UTCTime,
    appPermissions :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ClearDuesRes = ClearDuesRes
  { orderId :: Id DOrder.PaymentOrder,
    orderResp :: Payment.CreateOrderResp
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data GetCityReq = GetCityReq
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data GetCityResp = GetCityResp
  { city :: Maybe Text,
    status :: APISuccess
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getInformation ::
  ( KvDbFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Int ->
  m DriverInformationRes
getInformation (personId, merchantId, merchantOpCityId) mbToss = do
  let driverId = cast personId
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  driverStats <- runInReplica $ QDriverStats.findById driverId >>= fromMaybeM DriverInfoNotFound
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  driverReferralCode <- fmap (.referralCode) <$> QDR.findById (cast driverId)
  driverEntity <- buildDriverEntityRes (person, driverInfo)
  dues <- QDF.findAllPendingAndDueDriverFeeByDriverIdForServiceName driverId YATRI_SUBSCRIPTION
  let currentDues = sum $ map (\dueInvoice -> SLDriverFee.roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) dues
  let manualDues = sum $ map (\dueInvoice -> SLDriverFee.roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) $ filter (\due -> due.status == DDF.PAYMENT_OVERDUE) dues
  logDebug $ "alternateNumber-" <> show driverEntity.alternateNumber
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (.useCACForFrontend) systemConfigs
  frntndfgs <- if useCACConfig then getFrontendConfigs merchantOpCityId mbToss else return $ Just DAKM.empty
  let mbMd5Digest = T.pack . show . MD5.md5 . DA.encode <$> frntndfgs
  merchant <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  driverGoHomeInfo <- CQDGR.getDriverGoHomeRequestInfo driverId merchantOpCityId Nothing
  makeDriverInformationRes merchantOpCityId driverEntity merchant driverReferralCode driverStats driverGoHomeInfo (Just currentDues) (Just manualDues) mbMd5Digest

setActivity :: KvDbFlow m r => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Bool -> Maybe DriverInfo.DriverMode -> m APISuccess.APISuccess
setActivity (personId, _merchantId, merchantOpCityId) isActive mode = do
  void $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let driverId = cast personId
  when (isActive || (isJust mode && (mode == Just DriverInfo.SILENT || mode == Just DriverInfo.ONLINE))) $ do
    driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
    autoPayStatus <- fst <$> DAPlan.getSubcriptionStatusWithPlan Plan.YATRI_SUBSCRIPTION personId
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    freeTrialDaysLeft <- getFreeTrialDaysLeft transporterConfig.freeTrialDays driverInfo
    mbVehicle <- QV.findById personId
    let isEnableForVariant = maybe False (`elem` transporterConfig.variantsToEnableForSubscription) (mbVehicle <&> (.variant))
    let planBasedChecks = transporterConfig.isPlanMandatory && isNothing autoPayStatus && freeTrialDaysLeft <= 0 && not transporterConfig.allowDefaultPlanAllocation && isEnableForVariant
    when (isNothing mbVehicle) $ throwError (DriverWithoutVehicle personId.getId)
    when (planBasedChecks) $ throwError (NoPlanSelected personId.getId)
    unless (driverInfo.enabled) $ throwError DriverAccountDisabled
    unless (driverInfo.subscribed || transporterConfig.openMarketUnBlocked) $ throwError DriverUnsubscribed
    unless (not driverInfo.blocked) $ throwError DriverAccountBlocked
  void $ QDriverInformation.updateActivity isActive (mode <|> Just DriverInfo.OFFLINE) driverId
  pure APISuccess.Success

activateGoHomeFeature :: KvDbFlow m r => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id DDHL.DriverHomeLocation -> LatLong -> m APISuccess.APISuccess
activateGoHomeFeature (driverId, _merchantId, merchantOpCityId) driverHomeLocationId driverLocation = do
  goHomeConfig <- CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))
  unless (goHomeConfig.enableGoHome) $ throwError GoHomeFeaturePermanentlyDisabled
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  unless driverInfo.enabled $ throwError DriverAccountDisabled
  when (driverInfo.blocked) $ throwError DriverAccountBlocked
  let currPos = LatLong {lat = driverLocation.lat, lon = driverLocation.lon}
  driverHomeLocation <- QDHL.findById driverHomeLocationId >>= fromMaybeM (DriverHomeLocationDoesNotExist driverHomeLocationId.getId)
  when (driverHomeLocation.driverId /= driverId) $ throwError DriverHomeLocationDoesNotBelongToDriver
  let homePos = LatLong {lat = driverHomeLocation.lat, lon = driverHomeLocation.lon}
  unless (distanceBetweenInMeters homePos currPos > fromIntegral goHomeConfig.destRadiusMeters) $ throwError DriverCloseToHomeLocation
  dghInfo <- CQDGR.getDriverGoHomeRequestInfo driverId merchantOpCityId (Just goHomeConfig)
  whenM (fmap ((dghInfo.status == Just DDGR.ACTIVE) ||) (isJust <$> QDGR.findActive driverId)) $ throwError DriverGoHomeRequestAlreadyActive
  unless (dghInfo.cnt > 0) $ throwError DriverGoHomeRequestDailyUsageLimitReached
  activateDriverGoHomeRequest merchantOpCityId driverId driverHomeLocation goHomeConfig dghInfo
  pure APISuccess.Success

deactivateGoHomeFeature :: KvDbFlow m r => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> m APISuccess.APISuccess
deactivateGoHomeFeature (personId, _, merchantOpCityId) = do
  goHomeConfig <- CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId)))
  unless (goHomeConfig.enableGoHome) $ throwError GoHomeFeaturePermanentlyDisabled
  let driverId = cast personId
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  unless driverInfo.enabled $ throwError DriverAccountDisabled
  when (driverInfo.blocked) $ throwError DriverAccountBlocked
  ghInfo <- getDriverGoHomeRequestInfo driverId merchantOpCityId (Just goHomeConfig)
  ghrId <- fromMaybeM DriverGoHomeRequestNotPresent ghInfo.driverGoHomeRequestId
  succRide <- Ride.findCompletedRideByGHRId ghrId
  if isJust succRide
    then CQDGR.deactivateDriverGoHomeRequest merchantOpCityId driverId DDGR.SUCCESS ghInfo (Just False)
    else CQDGR.deactivateDriverGoHomeRequest merchantOpCityId driverId DDGR.FAILED ghInfo Nothing
  pure APISuccess.Success

addHomeLocation :: KvDbFlow m r => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> AddHomeLocationReq -> m APISuccess.APISuccess
addHomeLocation (driverId, merchantId, merchantOpCityId) req = do
  cfg <- CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))
  unless (cfg.enableGoHome) $ throwError GoHomeFeaturePermanentlyDisabled
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  unless driverInfo.enabled $ throwError DriverAccountDisabled
  when (driverInfo.blocked) $ throwError DriverAccountBlocked
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unlessM (rideServiceable merchant.geofencingConfig QGeometry.someGeometriesContain req.position Nothing) $ throwError DriverHomeLocationOutsideServiceArea
  oldHomeLocations <- QDHL.findAllByDriverId driverId
  unless (length oldHomeLocations < cfg.numHomeLocations) $ throwError DriverHomeLocationLimitReached
  when (any (\homeLocation -> highPrecMetersToMeters (distanceBetweenInMeters req.position (LatLong {lat = homeLocation.lat, lon = homeLocation.lon})) <= cfg.newLocAllowedRadius) oldHomeLocations) $ throwError NewLocationTooCloseToPreviousHomeLocation
  QDHL.create =<< buildDriverHomeLocation driverId req
  pure APISuccess.Success

buildDriverHomeLocation :: KvDbFlow m r => Id SP.Person -> AddHomeLocationReq -> m DDHL.DriverHomeLocation
buildDriverHomeLocation driverId req = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    DDHL.DriverHomeLocation
      { lat = req.position.lat,
        lon = req.position.lon,
        address = req.address,
        tag = req.tag,
        updatedAt = now,
        createdAt = now,
        ..
      }

updateHomeLocation :: KvDbFlow m r => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id DDHL.DriverHomeLocation -> UpdateHomeLocationReq -> m APISuccess.APISuccess
updateHomeLocation (driverId, merchantId, merchantOpCityId) homeLocationId req = do
  goHomeConfig <- CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))
  unless (goHomeConfig.enableGoHome) $ throwError GoHomeFeaturePermanentlyDisabled
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  unless driverInfo.enabled $ throwError DriverAccountDisabled
  unless (not driverInfo.blocked) $ throwError DriverAccountBlocked
  dghInfo <- CQDGR.getDriverGoHomeRequestInfo driverId merchantOpCityId (Just goHomeConfig)
  when (dghInfo.status == Just DDGR.ACTIVE) $ throwError DriverHomeLocationUpdateWhileActiveError
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unlessM (rideServiceable merchant.geofencingConfig QGeometry.someGeometriesContain req.position Nothing) $ throwError DriverHomeLocationOutsideServiceArea
  oldHomeLocation <- QDHL.findById homeLocationId >>= fromMaybeM (DriverHomeLocationDoesNotExist (T.pack "The given driver home location ID is invalid"))
  oldHomeLocations <- QDHL.findAllByDriverId driverId
  when (any (\homeLocation -> highPrecMetersToMeters (distanceBetweenInMeters req.position (LatLong {lat = homeLocation.lat, lon = homeLocation.lon})) <= goHomeConfig.newLocAllowedRadius) oldHomeLocations) $ throwError NewLocationTooCloseToPreviousHomeLocation
  currTime <- getCurrentTime
  when (diffUTCTime currTime oldHomeLocation.updatedAt < fromIntegral goHomeConfig.updateHomeLocationAfterSec) $ throwError DriverHomeLocationUpdateBeforeTime
  QDHL.updateHomeLocationById homeLocationId buildDriverHomeLocationUpdate
  return APISuccess.Success
  where
    buildDriverHomeLocationUpdate =
      DDHL.UpdateDriverHomeLocation
        { lat = req.position.lat,
          lon = req.position.lon,
          address = req.address,
          tag = req.tag
        }

getHomeLocations :: KvDbFlow m r => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> m GetHomeLocationsRes
getHomeLocations (driverId, _, _) = do
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  unless driverInfo.enabled $ throwError DriverAccountDisabled
  unless (not driverInfo.blocked) $ throwError DriverAccountBlocked
  driverHomeLocations <- QDHL.findAllByDriverId driverId
  return . GetHomeLocationsRes $ DDHL.makeDriverHomeLocationAPIEntity <$> driverHomeLocations

deleteHomeLocation :: KvDbFlow m r => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id DDHL.DriverHomeLocation -> m APISuccess.APISuccess
deleteHomeLocation (driverId, _, merchantOpCityId) driverHomeLocationId = do
  goHomeConfig <- CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))
  unless (goHomeConfig.enableGoHome) $ throwError GoHomeFeaturePermanentlyDisabled
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  unless driverInfo.enabled $ throwError DriverAccountDisabled
  unless (not driverInfo.blocked) $ throwError DriverAccountBlocked
  dghInfo <- CQDGR.getDriverGoHomeRequestInfo driverId merchantOpCityId (Just goHomeConfig)
  when (dghInfo.status == Just DDGR.ACTIVE) $ throwError DriverHomeLocationDeleteWhileActiveError
  QDHL.deleteById driverHomeLocationId
  return APISuccess.Success

buildDriverEntityRes :: (EsqDBReplicaFlow m r, EncFlow m r, CacheFlow m r, HasField "s3Env" r (S3.S3Env m), KvDbFlow m r) => (SP.Person, DriverInformation) -> m DriverEntityRes
buildDriverEntityRes (person, driverInfo) = do
  transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  driverPlan <- snd <$> DAPlan.getSubcriptionStatusWithPlan Plan.YATRI_SUBSCRIPTION person.id
  vehicleMB <- QVehicle.findById person.id
  now <- getCurrentTime
  decMobNum <- mapM decrypt person.mobileNumber
  decaltMobNum <- mapM decrypt person.alternateMobileNumber
  let maskedDeviceToken = maskText . (.getFCMRecipientToken) <$> person.deviceToken
  mediaUrl <- forM person.faceImageId $ \mediaId -> do
    mediaEntry <- runInReplica $ MFQuery.findById mediaId >>= fromMaybeM (FileDoNotExist person.id.getId)
    return mediaEntry.url
  aadhaarCardPhotoResp <- try @_ @SomeException (fetchAndCacheAadhaarImage person driverInfo)
  let aadhaarCardPhoto = join (eitherToMaybe aadhaarCardPhotoResp)
  freeTrialDaysLeft <- getFreeTrialDaysLeft transporterConfig.freeTrialDays driverInfo
  let rating =
        if transporterConfig.ratingAsDecimal
          then SP.roundToOneDecimal <$> person.rating
          else person.rating <&> (\(Centesimal x) -> Centesimal (fromInteger (round x)))
  fareProductConfig <- CQFP.findAllFareProductByMerchantOpCityId person.merchantOperatingCityId
  let supportedServiceTiers = nub $ map (.vehicleServiceTier) fareProductConfig
  (checkIfACWorking, mbDefaultServiceTier) <-
    case vehicleMB of
      Nothing -> return (False, Nothing)
      Just vehicle -> do
        cityServiceTiers <- CQVST.findAllByMerchantOpCityId person.merchantOperatingCityId
        let mbDefaultServiceTierItem = find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
        let checIfACWorking' =
              case mbDefaultServiceTierItem >>= (.airConditioned) of
                Nothing -> False
                Just acThreshold -> do
                  (fromMaybe 0 driverInfo.airConditionScore) <= acThreshold
                    && maybe True (\lastCheckedAt -> fromInteger (diffDays (utctDay now) (utctDay lastCheckedAt)) >= transporterConfig.acStatusCheckGap) driverInfo.lastACStatusCheckedAt
        return (checIfACWorking', (.serviceTierType) <$> mbDefaultServiceTierItem)
  let isVehicleSupported = maybe False (\d -> d `elem` supportedServiceTiers) mbDefaultServiceTier
  return $
    DriverEntityRes
      { id = person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        mobileNumber = decMobNum,
        rating,
        linkedVehicle = makeVehicleAPIEntity mbDefaultServiceTier <$> vehicleMB,
        active = driverInfo.active,
        onRide = driverInfo.onRide,
        enabled = driverInfo.enabled,
        blocked = driverInfo.blocked,
        verified = driverInfo.verified,
        subscribed = driverInfo.subscribed,
        paymentPending = driverInfo.paymentPending,
        registeredAt = person.createdAt,
        language = person.language,
        alternateNumber = decaltMobNum,
        canDowngradeToSedan = driverInfo.canDowngradeToSedan,
        canDowngradeToHatchback = driverInfo.canDowngradeToHatchback,
        canDowngradeToTaxi = driverInfo.canDowngradeToTaxi,
        canSwitchToRental = driverInfo.canSwitchToRental,
        canSwitchToInterCity = driverInfo.canSwitchToInterCity,
        mode = driverInfo.mode,
        payerVpa = driverPlan >>= (.payerVpa),
        blockStateModifier = driverInfo.blockStateModifier,
        autoPayStatus = driverPlan >>= (.autoPayStatus),
        clientVersion = person.clientSdkVersion,
        bundleVersion = person.clientBundleVersion,
        gender = Just person.gender,
        mediaUrl = mediaUrl,
        aadhaarCardPhoto = aadhaarCardPhoto,
        freeTrialDaysLeft = freeTrialDaysLeft,
        maskedDeviceToken = maskedDeviceToken,
        checkIfACWorking,
        isVehicleSupported = isVehicleSupported
      }

deleteDriver :: (KvDbFlow m r, Redis.HedisFlow m r, MonadReader r m) => SP.Person -> Id SP.Person -> m APISuccess
deleteDriver admin driverId = do
  driver <-
    QPerson.findById driverId
      >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  unless (driver.merchantId == admin.merchantId || driver.role == SP.DRIVER) $ throwError Unauthorized
  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession driverId
  QDriverInformation.deleteById (cast driverId)
  QDriverStats.deleteById (cast driverId)
  QR.deleteByPersonId driverId.getId
  QVehicle.deleteById driverId
  QDHL.deleteByDriverId driverId
  QPerson.deleteById driverId
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> deleteDriver : ") (show driverId)
  return Success

updateDriver ::
  ( KvDbFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasField "s3Env" r (S3.S3Env m),
    HasField "version" r DeploymentVersion
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  UpdateDriverReq ->
  m UpdateDriverRes
updateDriver (personId, _, merchantOpCityId) mbBundleVersion mbClientVersion mbConfigVersion mbDevice req = do
  runRequestValidation validateUpdateDriverReq req
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  deploymentVersion <- asks (.version)
  let updPerson =
        person{firstName = fromMaybe person.firstName req.firstName,
               middleName = req.middleName <|> person.middleName,
               lastName = req.lastName <|> person.lastName,
               deviceToken = req.deviceToken <|> person.deviceToken,
               language = req.language <|> person.language,
               clientSdkVersion = mbClientVersion <|> person.clientSdkVersion,
               clientBundleVersion = mbBundleVersion <|> person.clientBundleVersion,
               clientConfigVersion = mbConfigVersion <|> person.clientConfigVersion,
               clientDevice = getDeviceFromText mbDevice <|> person.clientDevice,
               backendConfigVersion = person.backendConfigVersion,
               backendAppVersion = Just deploymentVersion.getDeploymentVersion,
               gender = fromMaybe person.gender req.gender,
               hometown = req.hometown <|> person.hometown,
               languagesSpoken = req.languagesSpoken <|> person.languagesSpoken
              }
  mVehicle <- QVehicle.findById personId
  driverInfo <- QDriverInformation.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  whenJust mVehicle $ \vehicle -> do
    when (isJust req.canDowngradeToSedan || isJust req.canDowngradeToHatchback || isJust req.canDowngradeToTaxi || isJust req.canSwitchToRental || isJust req.canSwitchToInterCity) $ do
      -- deprecated logic, moved to driver service tier options
      checkIfCanDowngrade vehicle
      let canDowngradeToSedan = fromMaybe driverInfo.canDowngradeToSedan req.canDowngradeToSedan
          canDowngradeToHatchback = fromMaybe driverInfo.canDowngradeToHatchback req.canDowngradeToHatchback
          canDowngradeToTaxi = fromMaybe driverInfo.canDowngradeToTaxi req.canDowngradeToTaxi
          canSwitchToRental = fromMaybe driverInfo.canSwitchToRental req.canSwitchToRental
          canSwitchToInterCity = fromMaybe driverInfo.canSwitchToInterCity req.canSwitchToInterCity
          availableUpiApps = req.availableUpiApps <|> driverInfo.availableUpiApps
          selectedServiceTiers =
            case vehicle.variant of
              SV.AUTO_RICKSHAW -> [DVST.AUTO_RICKSHAW]
              SV.TAXI -> [DVST.TAXI]
              SV.HATCHBACK -> [DVST.HATCHBACK, DVST.ECO] <> [DVST.TAXI | canDowngradeToTaxi]
              SV.SEDAN -> [DVST.SEDAN, DVST.COMFY] <> [DVST.HATCHBACK | canDowngradeToHatchback] <> [DVST.TAXI | canDowngradeToTaxi] <> [DVST.ECO | canDowngradeToHatchback]
              SV.SUV -> [DVST.SUV] <> [DVST.SEDAN | canDowngradeToSedan] <> [DVST.COMFY | canDowngradeToSedan] <> [DVST.HATCHBACK | canDowngradeToHatchback] <> [DVST.TAXI | canDowngradeToTaxi] <> [DVST.ECO | canDowngradeToHatchback]
              SV.TAXI_PLUS -> [DVST.TAXI_PLUS]
              SV.BIKE -> [DVST.BIKE]

      QDriverInformation.updateDriverInformation canDowngradeToSedan canDowngradeToHatchback canDowngradeToTaxi canSwitchToRental canSwitchToInterCity availableUpiApps person.id
      when (isJust req.canDowngradeToSedan || isJust req.canDowngradeToHatchback || isJust req.canDowngradeToTaxi) $
        QVehicle.updateSelectedServiceTiers selectedServiceTiers person.id

  updatedDriverInfo <- QDriverInformation.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  when (isJust req.vehicleName) $ QVehicle.updateVehicleName req.vehicleName personId
  QPerson.updatePersonRec personId updPerson
  driverStats <- runInReplica $ QDriverStats.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  driverEntity <- buildDriverEntityRes (updPerson, updatedDriverInfo)
  driverReferralCode <- fmap (.referralCode) <$> QDR.findById personId
  let merchantId = person.merchantId
  org <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  driverGoHomeInfo <- CQDGR.getDriverGoHomeRequestInfo personId merchantOpCityId Nothing
  makeDriverInformationRes merchantOpCityId driverEntity org driverReferralCode driverStats driverGoHomeInfo Nothing Nothing Nothing
  where
    -- logic is deprecated, should be handle from driver service tier options now, kept it for backward compatibility
    checkIfCanDowngrade vehicle = do
      when
        ( (vehicle.variant == SV.AUTO_RICKSHAW || vehicle.variant == SV.TAXI || vehicle.variant == SV.HATCHBACK)
            && (req.canDowngradeToSedan == Just True || req.canDowngradeToHatchback == Just True)
        )
        $ throwError $ InvalidRequest $ "Can't downgrade from " <> (show vehicle.variant)
      when (vehicle.variant == SV.SUV && req.canDowngradeToTaxi == Just True) $
        throwError $ InvalidRequest $ "Can't downgrade to NON-AC TAXI from " <> (show vehicle.variant)
      when
        ( (vehicle.variant == SV.AUTO_RICKSHAW || vehicle.variant == SV.TAXI)
            && (req.canDowngradeToSedan == Just True || req.canDowngradeToHatchback == Just True || req.canDowngradeToTaxi == Just True)
        )
        $ throwError $ InvalidRequest $ "Can't downgrade from " <> (show vehicle.variant)
      when (vehicle.variant == SV.SEDAN && (req.canDowngradeToSedan == Just True)) $
        throwError $ InvalidRequest "Driver with sedan can't downgrade to sedan"
      when (vehicle.variant == SV.TAXI_PLUS && (req.canDowngradeToSedan == Just True || req.canDowngradeToHatchback == Just True)) $
        throwError $ InvalidRequest "Driver with TAXI_PLUS can't downgrade to either sedan or hatchback"

updateMetaData ::
  ( KvDbFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  MetaDataReq ->
  m APISuccess
updateMetaData (personId, _, _) req = do
  void $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  QMeta.updateMetaData req.device req.deviceOS req.deviceDateTime req.appPermissions personId
  return Success

makeDriverInformationRes :: KvDbFlow m r => Id DMOC.MerchantOperatingCity -> DriverEntityRes -> DM.Merchant -> Maybe (Id DR.DriverReferral) -> DriverStats -> DDGR.CachedGoHomeRequest -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe Text -> m DriverInformationRes
makeDriverInformationRes merchantOpCityId DriverEntityRes {..} org referralCode driverStats dghInfo currentDues manualDues md5DigestHash = do
  merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast id))) >>= \cfg ->
    return $
      DriverInformationRes
        { organization = DM.makeMerchantAPIEntity org,
          referralCode = referralCode <&> (.getId),
          numberOfRides = driverStats.totalRides,
          driverGoHomeInfo = dghInfo,
          isGoHomeEnabled = cfg.enableGoHome,
          operatingCity = merchantOperatingCity.city,
          frontendConfigHash = md5DigestHash,
          currentDuesWithCurrency = flip PriceAPIEntity merchantOperatingCity.currency <$> currentDues,
          manualDuesWithCurrency = flip PriceAPIEntity merchantOperatingCity.currency <$> manualDues,
          ..
        }

getNearbySearchRequests ::
  ( KvDbFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe (Id DST.SearchTry) ->
  m GetNearbySearchRequestsRes
getNearbySearchRequests (driverId, _, merchantOpCityId) searchTryIdReq = do
  nearbyReqs <- runInReplica $ QSRD.findByDriver driverId
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let cancellationScoreRelatedConfig = mkCancellationScoreRelatedConfig transporterConfig
  cancellationRatio <- DP.getLatestCancellationRatio cancellationScoreRelatedConfig merchantOpCityId (cast driverId)
  searchRequestForDriverAPIEntity <- mapM (buildSearchRequestForDriverAPIEntity cancellationRatio cancellationScoreRelatedConfig transporterConfig) nearbyReqs
  case searchTryIdReq of
    Just stid -> do
      let filteredSearchRequestForDriverAPIEntity = filter (\srfd -> srfd.searchTryId == stid) searchRequestForDriverAPIEntity
      return $ GetNearbySearchRequestsRes filteredSearchRequestForDriverAPIEntity
    Nothing -> return $ GetNearbySearchRequestsRes searchRequestForDriverAPIEntity
  where
    buildSearchRequestForDriverAPIEntity cancellationRatio cancellationScoreRelatedConfig transporterConfig nearbyReq = do
      let searchTryId = nearbyReq.searchTryId
      searchTry <- runInReplica $ QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
      searchRequest <- runInReplica $ QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
      bapMetadata <- CQSM.findById (Id searchRequest.bapId)
      isValueAddNP <- CQVAN.isValueAddNP searchRequest.bapId
      farePolicy <- getFarePolicyByEstOrQuoteId searchRequest.merchantOperatingCityId searchTry.tripCategory nearbyReq.vehicleServiceTier searchRequest.area (fromMaybe searchTry.estimateId nearbyReq.estimateId) (Just (TransactionId (Id searchRequest.transactionId)))
      popupDelaySeconds <- DP.getPopupDelay merchantOpCityId (cast driverId) cancellationRatio cancellationScoreRelatedConfig transporterConfig.defaultPopupDelay
      let driverPickUpCharges = USRD.extractDriverPickupCharges farePolicy.farePolicyDetails
      return $ USRD.makeSearchRequestForDriverAPIEntity nearbyReq searchRequest searchTry bapMetadata popupDelaySeconds Nothing (Seconds 0) nearbyReq.vehicleServiceTier False isValueAddNP driverPickUpCharges -- Seconds 0 as we don't know where he/she lies within the driver pool, anyways this API is not used in prod now.
    mkCancellationScoreRelatedConfig :: TransporterConfig -> CancellationScoreRelatedConfig
    mkCancellationScoreRelatedConfig tc = CancellationScoreRelatedConfig tc.popupDelayToAddAsPenalty tc.thresholdCancellationScore tc.minRidesForCancellationScore

isAllowedExtraFee :: DriverExtraFeeBounds -> HighPrecMoney -> Bool
isAllowedExtraFee extraFee val = extraFee.minFee <= val && val <= extraFee.maxFee

offerQuoteLockKey :: Id Person -> Text
offerQuoteLockKey driverId = "Driver:OfferQuote:DriverId-" <> driverId.getId

-- DEPRECATED
offerQuote :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe (Id DC.Client) -> DriverOfferReq -> Flow APISuccess
offerQuote (driverId, merchantId, merchantOpCityId) clientId DriverOfferReq {..} = do
  let response = Accept
  respondQuote (driverId, merchantId, merchantOpCityId) clientId Nothing Nothing Nothing Nothing DriverRespondReq {searchRequestId = Nothing, searchTryId = Just searchRequestId, ..}

respondQuote :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe (Id DC.Client) -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> DriverRespondReq -> Flow APISuccess
respondQuote (driverId, merchantId, merchantOpCityId) clientId mbBundleVersion mbClientVersion mbConfigVersion mbDevice req = do
  Redis.whenWithLockRedis (offerQuoteLockKey driverId) 60 $ do
    let reqOfferedValue = (req.offeredFareWithCurrency <&> (.amount)) <|> (toHighPrecMoney <$> req.offeredFare)
    searchTryId <- req.searchRequestId <|> req.searchTryId & fromMaybeM (InvalidRequest "searchTryId field is not present.")
    searchTry <- QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
    SMerchant.checkCurrencies searchTry.currency [req.offeredFareWithCurrency]
    now <- getCurrentTime
    when (searchTry.validTill < now) $ throwError SearchRequestExpired
    searchReq <- QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
    merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantDoesNotExist searchReq.providerId.getId)
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    driverInfo <- QDriverInformation.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
    when driverInfo.onRide $ throwError DriverOnRide
    mSReqFD <- QSRD.findByDriverAndSearchTryId driverId searchTry.id
    sReqFD <-
      case mSReqFD of
        Just srfd -> return srfd
        Nothing -> do
          logError $ "Search request not found for the driver with driverId " <> driverId.getId <> " and searchTryId " <> searchTryId.getId
          throwError RideRequestAlreadyAccepted
    when (sReqFD.response == Just Reject) (throwError QuoteAlreadyRejected)
    driverFCMPulledList <-
      case req.response of
        Pulled -> do
          QSRD.updateDriverResponse (Just Pulled) Inactive sReqFD.id
          throwError UnexpectedResponseValue
        Accept -> do
          whenM thereAreActiveQuotes (throwError FoundActiveQuotes)
          pullList <-
            case searchTry.tripCategory of
              DTC.OneWay DTC.OneWayOnDemandDynamicOffer -> acceptDynamicOfferDriverRequest merchant searchTry searchReq driver sReqFD mbBundleVersion mbClientVersion mbConfigVersion mbDevice reqOfferedValue
              _ -> acceptStaticOfferDriverRequest searchTry driver sReqFD reqOfferedValue
          QSRD.updateDriverResponse (Just Accept) Inactive sReqFD.id
          return pullList
        Reject -> do
          QSRD.updateDriverResponse (Just Reject) Inactive sReqFD.id
          pure []
    DS.driverScoreEventHandler merchantOpCityId $ buildDriverRespondEventPayload searchTry.id driverFCMPulledList
  pure Success
  where
    buildDriverRespondEventPayload searchTryId restActiveDriverSearchReqs =
      DST.OnDriverAcceptingSearchRequest
        { restDriverIds = map (.driverId) restActiveDriverSearchReqs,
          response = req.response,
          ..
        }

    buildDriverQuote ::
      (MonadFlow m, MonadReader r m, HasField "driverQuoteExpirationSeconds" r NominalDiffTime, HasField "version" r DeploymentVersion) =>
      SP.Person ->
      DSR.SearchRequest ->
      SearchRequestForDriver ->
      Text ->
      DTC.TripCategory ->
      Fare.FareParameters ->
      Maybe Version ->
      Maybe Version ->
      Maybe Version ->
      Maybe Text ->
      m DDrQuote.DriverQuote
    buildDriverQuote driver searchReq sd estimateId tripCategory fareParams mbBundleVersion' mbClientVersion' mbConfigVersion' mbDevice' = do
      guid <- generateGUID
      now <- getCurrentTime
      deploymentVersion <- asks (.version)
      driverQuoteExpirationSeconds <- asks (.driverQuoteExpirationSeconds)
      let estimatedFare = fareSum fareParams
      pure
        DDrQuote.DriverQuote
          { id = guid,
            requestId = searchReq.id,
            searchTryId = sd.searchTryId,
            searchRequestForDriverId = Just sd.id,
            clientId = clientId,
            driverId,
            driverName = driver.firstName,
            driverRating = SP.roundToOneDecimal <$> driver.rating,
            status = DDrQuote.Active,
            vehicleVariant = sd.vehicleVariant,
            vehicleServiceTier = sd.vehicleServiceTier,
            distance = searchReq.estimatedDistance,
            distanceToPickup = sd.actualDistanceToPickup,
            durationToPickup = sd.durationToPickup,
            currency = sd.currency,
            createdAt = now,
            updatedAt = now,
            validTill = addUTCTime driverQuoteExpirationSeconds now,
            providerId = searchReq.providerId,
            estimatedFare,
            fareParams,
            specialLocationTag = searchReq.specialLocationTag,
            goHomeRequestId = sd.goHomeRequestId,
            tripCategory = tripCategory,
            estimateId = Id estimateId,
            clientSdkVersion = mbClientVersion',
            clientBundleVersion = mbBundleVersion',
            clientConfigVersion = mbConfigVersion',
            clientDevice = getDeviceFromText mbDevice',
            backendConfigVersion = Nothing,
            backendAppVersion = Just deploymentVersion.getDeploymentVersion
          }
    thereAreActiveQuotes = do
      driverUnlockDelay <- asks (.driverUnlockDelay)
      activeQuotes <- QDrQt.findActiveQuotesByDriverId driverId driverUnlockDelay
      logDebug $ "active quotes for driverId = " <> driverId.getId <> show activeQuotes
      pure $ not $ null activeQuotes
    getQuoteLimit dist vehicleServiceTier tripCategory txnId area = do
      driverPoolCfg <- SCDPC.getDriverPoolConfig merchantOpCityId vehicleServiceTier tripCategory area dist (Just (TransactionId (Id txnId)))
      pure driverPoolCfg.driverQuoteLimit

    acceptDynamicOfferDriverRequest :: DM.Merchant -> DST.SearchTry -> DSR.SearchRequest -> SP.Person -> SearchRequestForDriver -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe HighPrecMoney -> Flow [SearchRequestForDriver]
    acceptDynamicOfferDriverRequest merchant searchTry searchReq driver sReqFD mbBundleVersion' mbClientVersion' mbConfigVersion' mbDevice' reqOfferedValue = do
      let estimateId = fromMaybe searchTry.estimateId sReqFD.estimateId -- backward compatibility
      when (searchReq.autoAssignEnabled == Just True) $ do
        unlessM (CS.lockSearchTry searchTry.id) $
          throwError (InternalError "SEARCH_TRY_CANCELLED")
      logDebug $ "offered fare: " <> show reqOfferedValue
      quoteLimit <- getQuoteLimit searchReq.estimatedDistance sReqFD.vehicleServiceTier searchTry.tripCategory searchReq.transactionId (fromMaybe SL.Default searchReq.area)
      quoteCount <- runInReplica $ QDrQt.countAllBySTId searchTry.id
      when (quoteCount >= quoteLimit) (throwError QuoteAlreadyRejected)
      farePolicy <- getFarePolicyByEstOrQuoteId merchantOpCityId searchTry.tripCategory sReqFD.vehicleServiceTier searchReq.area estimateId (Just (TransactionId (Id searchReq.transactionId)))
      let driverExtraFeeBounds = DFarePolicy.findDriverExtraFeeBoundsByDistance (fromMaybe 0 searchReq.estimatedDistance) <$> farePolicy.driverExtraFeeBounds
      whenJust reqOfferedValue $ \off ->
        whenJust driverExtraFeeBounds $ \driverExtraFeeBounds' ->
          unless (isAllowedExtraFee driverExtraFeeBounds' off) $
            throwError $ NotAllowedExtraFee $ show off
      fareParams <- do
        calculateFareParameters
          CalculateFareParametersParams
            { farePolicy = farePolicy,
              actualDistance = searchReq.estimatedDistance,
              rideTime = sReqFD.startTime,
              waitingTime = Nothing,
              actualRideDuration = Nothing,
              avgSpeedOfVehicle = Nothing,
              driverSelectedFare = reqOfferedValue,
              customerExtraFee = searchTry.customerExtraFee,
              nightShiftCharge = Nothing,
              customerCancellationDues = searchReq.customerCancellationDues,
              tollCharges = searchReq.tollCharges,
              estimatedRideDuration = Nothing,
              nightShiftOverlapChecking = DTC.isRentalTrip searchTry.tripCategory,
              estimatedDistance = Nothing,
              timeDiffFromUtc = Nothing,
              currency = searchReq.currency,
              ..
            }
      QFP.updateFareParameters fareParams
      driverQuote <- buildDriverQuote driver searchReq sReqFD estimateId searchTry.tripCategory fareParams mbBundleVersion' mbClientVersion' mbConfigVersion' mbDevice'
      void $ cacheFarePolicyByQuoteId driverQuote.id.getId farePolicy
      triggerQuoteEvent QuoteEventData {quote = driverQuote}
      void $ QDrQt.create driverQuote
      driverFCMPulledList <-
        if (quoteCount + 1) >= quoteLimit || (searchReq.autoAssignEnabled == Just True)
          then QSRD.findAllActiveBySTId searchTry.id DSRD.Active
          else pure []
      pullExistingRideRequests merchantOpCityId driverFCMPulledList merchantId driver.id $ mkPrice (Just driverQuote.currency) driverQuote.estimatedFare
      sendDriverOffer merchant searchReq sReqFD searchTry driverQuote
      return driverFCMPulledList

    acceptStaticOfferDriverRequest searchTry driver sReqFD reqOfferedValue = do
      let quoteId = fromMaybe searchTry.estimateId sReqFD.estimateId -- backward compatibility
      whenJust reqOfferedValue $ \_ -> throwError (InvalidRequest "Driver can't offer rental fare")
      quote <- QQuote.findById (Id quoteId) >>= fromMaybeM (QuoteNotFound quoteId)
      booking <- QBooking.findByQuoteId quote.id.getId >>= fromMaybeM (BookingDoesNotExist quote.id.getId)
      isBookingAssignmentInprogress' <- CS.isBookingAssignmentInprogress booking.id
      when isBookingAssignmentInprogress' $ throwError RideRequestAlreadyAccepted
      isBookingCancelled' <- CS.isBookingCancelled booking.id
      when isBookingCancelled' $ throwError (InternalError "BOOKING_CANCELLED")
      CS.markBookingAssignmentInprogress booking.id -- this is to handle booking assignment and user cancellation at same time
      unless (booking.status == DRB.NEW) $ throwError RideRequestAlreadyAccepted
      QST.updateStatus DST.COMPLETED searchTry.id
      (ride, _, vehicle) <- initializeRide merchantId driver booking Nothing Nothing clientId
      driverFCMPulledList <- deactivateExistingQuotes merchantOpCityId merchantId driver.id searchTry.id $ mkPrice (Just quote.currency) quote.estimatedFare
      void $ sendRideAssignedUpdateToBAP booking ride driver vehicle
      CS.markBookingAssignmentCompleted booking.id
      return driverFCMPulledList

getStats ::
  (EsqDBReplicaFlow m r, KvDbFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Day ->
  m DriverStatsRes
getStats (driverId, _, merchantOpCityId) date = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  rides <- runInReplica $ QRide.getRidesForDate driverId date transporterConfig.timeDiffFromUtc
  let fareParamId = mapMaybe (.fareParametersId) rides
  fareParameters <- (runInReplica . QFP.findAllIn) fareParamId
  coinBalance_ <- Coins.getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  validRideCountOfDriver <- fromMaybe 0 <$> Coins.getValidRideCountByDriverIdKey driverId
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId

  let (driverSelFares, customerExtFees) = (mapMaybe (.driverSelectedFare) fareParameters, mapMaybe (.customerExtraFee) fareParameters)
      deadKmFares =
        mapMaybe
          ( \x -> case fareParametersDetails x of
              ProgressiveDetails det -> Just (deadKmFare det)
              SlabDetails _ -> Nothing
              RentalDetails _ -> Nothing
          )
          fareParameters
  let bonusEarning = GHCL.sum driverSelFares + GHCL.sum customerExtFees + GHCL.sum deadKmFares
  let totalEarningsOfDay = sum (mapMaybe (.fare) rides)
      totalDistanceTravelledInKilometers = sum (mapMaybe (.chargeableDistance) rides) `div` 1000
      totalEarningOfDayExcludingTollCharges = totalEarningsOfDay - (sum (mapMaybe (.tollCharges) rides) :: HighPrecMoney)
      totalEarningsOfDayPerKm =
        if totalDistanceTravelledInKilometers.getMeters == 0
          then HighPrecMoney 0.0
          else toHighPrecMoney $ roundToIntegral totalEarningOfDayExcludingTollCharges `div` totalDistanceTravelledInKilometers.getMeters
  return $
    DriverStatsRes
      { coinBalance = coinBalance_,
        totalRidesOfDay = length rides,
        totalEarningsOfDay = roundToIntegral totalEarningsOfDay,
        totalEarningsOfDayWithCurrency = PriceAPIEntity totalEarningsOfDay currency,
        totalValidRidesOfDay = validRideCountOfDriver,
        totalEarningsOfDayPerKm = roundToIntegral totalEarningsOfDayPerKm,
        totalEarningsOfDayPerKmWithCurrency = PriceAPIEntity totalEarningsOfDayPerKm currency,
        bonusEarning = roundToIntegral bonusEarning,
        bonusEarningWithCurrency = PriceAPIEntity bonusEarning currency
      }

driverPhotoUploadHitsCountKey :: Id SP.Person -> Text
driverPhotoUploadHitsCountKey driverId = "BPP:ProfilePhoto:verify:" <> getId driverId <> ":hitsCount"

driverPhotoUpload :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> DriverPhotoUploadReq -> Flow APISuccess
driverPhotoUpload (driverId, merchantId, merchantOpCityId) DriverPhotoUploadReq {..} = do
  checkSlidingWindowLimit (driverPhotoUploadHitsCountKey driverId)
  person <- runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonNotFound (getId driverId))
  imageExtension <- validateContentType
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when transporterConfig.enableFaceVerification
    let req = IF.FaceValidationReq {file = image, brisqueFeatures}
     in void $ validateFaceImage merchantId merchantOpCityId req
  filePath <- S3.createFilePath "/driver-profile-picture/" ("driver-" <> getId driverId) fileType imageExtension
  let fileUrl =
        transporterConfig.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "driver/profile/photo"
          & T.replace "<FILE_PATH>" filePath
  result <- try @_ @SomeException $ S3.put (T.unpack filePath) image
  case result of
    Left err -> throwError $ InternalError ("S3 Upload Failed: " <> show err)
    Right _ -> do
      case person.faceImageId of
        Just mediaFileId -> do
          QPerson.updateMediaId driverId Nothing
          MFQuery.deleteById mediaFileId
        Nothing -> return ()
  createMediaEntry driverId Common.AddLinkAsMedia {url = fileUrl, fileType}
  where
    validateContentType = do
      case fileType of
        S3.Image -> case reqContentType of
          "image/png" -> pure "png"
          "image/jpeg" -> pure "jpg"
          _ -> throwError $ FileFormatNotSupported reqContentType
        _ -> throwError $ FileFormatNotSupported reqContentType

fetchDriverPhoto :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> Flow Text
fetchDriverPhoto _ filePath = S3.get $ T.unpack filePath

createMediaEntry :: Id SP.Person -> Common.AddLinkAsMedia -> Flow APISuccess
createMediaEntry driverId Common.AddLinkAsMedia {..} = do
  fileEntity <- mkFile url
  MFQuery.create fileEntity
  QPerson.updateMediaId driverId (Just fileEntity.id)
  return Success
  where
    mkFile fileUrl = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        Domain.MediaFile
          { id,
            _type = S3.Image,
            url = fileUrl,
            createdAt = now
          }

makeAlternatePhoneNumberKey :: Id SP.Person -> Text
makeAlternatePhoneNumberKey id = "DriverAlternatePhoneNumber:PersonId-" <> id.getId

makeAlternateNumberOtpKey :: Id SP.Person -> Text
makeAlternateNumberOtpKey id = "DriverAlternateNumberOtp:PersonId-" <> id.getId

makeAlternateNumberAttemptsKey :: Id SP.Person -> Text
makeAlternateNumberAttemptsKey id = "DriverAlternateNumberAttempts:PersonId-" <> id.getId

makeAlternateNumberVerifiedKey :: Id SP.Person -> Text
makeAlternateNumberVerifiedKey id = "DriverAlternateNumberVerified:PersonId-" <> id.getId

cacheAlternateNumberInfo :: (CacheFlow m r) => Id SP.Person -> Text -> Text -> Int -> Bool -> m ()
cacheAlternateNumberInfo personId phoneNumber otp attemptsLeft verified = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let alternatePhoneNumberKey = makeAlternatePhoneNumberKey personId
      alternateNumberOtpKey = makeAlternateNumberOtpKey personId
      alternateNumberAttemptsKey = makeAlternateNumberAttemptsKey personId
      alternateNumberVerifiedKey = makeAlternateNumberVerifiedKey personId

  Redis.setExp alternatePhoneNumberKey phoneNumber expTime
  Redis.setExp alternateNumberOtpKey otp expTime
  Redis.setExp alternateNumberAttemptsKey attemptsLeft expTime
  Redis.setExp alternateNumberVerifiedKey verified expTime

invalidateAlternateNoCache :: (CacheFlow m r) => Id SP.Person -> m ()
invalidateAlternateNoCache personId = do
  let alternatePhoneNumberKey = makeAlternatePhoneNumberKey personId
      alternateNumberOtpKey = makeAlternateNumberOtpKey personId
      alternateNumberAttemptsKey = makeAlternateNumberAttemptsKey personId

  Redis.del alternatePhoneNumberKey
  Redis.del alternateNumberOtpKey
  Redis.del alternateNumberAttemptsKey

validationCheck :: Validate DriverAlternateNumberReq
validationCheck DriverAlternateNumberReq {..} = do
  sequenceA_
    [ validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode,
      validateField "alternateNumber" alternateNumber P.mobileNumber
    ]

validate ::
  ( KvDbFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig]
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverAlternateNumberReq ->
  m DriverAlternateNumberRes
validate (personId, _, merchantOpCityId) phoneNumber = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  altNoAttempt <- runInReplica $ QRegister.getAlternateNumberAttempts personId
  runRequestValidation validationCheck phoneNumber
  mobileNumberHash <- getDbHash phoneNumber.alternateNumber
  merchant <- CQM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  mbPerson <- QPerson.findByMobileNumberAndMerchantAndRole phoneNumber.mobileCountryCode mobileNumberHash person.merchantId DRIVER
  deleteOldPersonCheck <- case mbPerson of
    Nothing -> return False
    Just oldPerson -> do
      when (oldPerson.id == person.id) $ throwError $ InvalidRequest "Alternate number already linked"
      DeleteDriverOnCheck.validateDriver merchant oldPerson
  logDebug $ "Delete Driver Check" <> show deleteOldPersonCheck
  when deleteOldPersonCheck $ throwError $ InvalidRequest "Alternate number can't be validated"
  smsCfg <- asks (.smsCfg)
  let useFakeOtpM = useFakeSms smsCfg
  otpCode <- maybe generateOTPCode (return . show) useFakeOtpM
  whenNothing_ useFakeOtpM $ do
    let otpHash = smsCfg.credConfig.otpHash
    let altPhoneNumber = phoneNumber.mobileCountryCode <> phoneNumber.alternateNumber
    let sender = smsCfg.sender
    withLogTag ("personId_" <> getId person.id) $ do
      message <-
        MessageBuilder.buildSendAlternateNumberOTPMessage merchantOpCityId $
          MessageBuilder.BuildSendOTPMessageReq
            { otp = otpCode,
              hash = otpHash
            }
      Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message altPhoneNumber sender)
        >>= Sms.checkSmsResult
  let verified = False
  cacheAlternateNumberInfo personId phoneNumber.alternateNumber otpCode altNoAttempt verified
  return $ DriverAlternateNumberRes {attempts = altNoAttempt}

validateAuthVerifyReq :: Validate DriverAlternateNumberOtpReq
validateAuthVerifyReq DriverAlternateNumberOtpReq {..} =
  sequenceA_
    [ validateField "otp" otp $ ExactLength 4 `And` star P.digit
    ]

verifyHitsCountKey :: Id SP.Person -> Text
verifyHitsCountKey id = "Driver:AlternateNumberOtp:verify:" <> getId id <> ":hitsCount"

verifyAuth ::
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverAlternateNumberOtpReq ->
  Flow APISuccess
verifyAuth (personId, _, _) req = do
  Redis.whenWithLockRedis (makeAlternatePhoneNumberKey personId) 60 $ do
    runRequestValidation validateAuthVerifyReq req
    person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    checkSlidingWindowLimit (verifyHitsCountKey personId)
    verified <- Redis.get (makeAlternateNumberVerifiedKey personId) >>= fromMaybeM (InvalidRequest "Verified not found")
    when verified $ throwError $ AuthBlocked "Already verified."
    altMobNo <- Redis.get (makeAlternatePhoneNumberKey personId) >>= fromMaybeM (InvalidRequest "Alternate Number not found")
    val <- Redis.get (makeAlternateNumberOtpKey personId)
    authValueHash <- case val of
      Nothing -> throwError $ InternalError "Auth not found"
      Just a -> return a
    unless (authValueHash == req.otp) $ throwError InvalidAuthData
    encNewNum <- encrypt altMobNo
    let driver =
          person
            { SP.unencryptedAlternateMobileNumber = Just altMobNo,
              SP.alternateMobileNumber = Just encNewNum
            }
        mobileCountryCode = fromMaybe "+91" person.mobileCountryCode
    mobileNumberHash <- getDbHash altMobNo
    mbPerson <- QPerson.findByMobileNumberAndMerchantAndRole mobileCountryCode mobileNumberHash person.merchantId SP.DRIVER
    void $ QPerson.updateAlternateMobileNumberAndCode driver
    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
    Redis.setExp (makeAlternateNumberVerifiedKey personId) True expTime
    whenJust mbPerson $ \oldPerson -> do
      merchant <- CQM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
      void $ DeleteDriverOnCheck.deleteDriver merchant.shortId (cast oldPerson.id)
    invalidateAlternateNoCache personId
  return Success

resendOtp ::
  ( HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    KvDbFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CacheFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverAlternateNumberReq ->
  m ResendAuth
resendOtp (personId, merchantId, merchantOpCityId) req = do
  attemptsLeft :: Int <- do
    res <- Redis.get (makeAlternateNumberAttemptsKey personId)
    return $ fromMaybe 0 res
  logDebug $ "attemptsLeft" <> show attemptsLeft
  unless (attemptsLeft > 0) $ throwError $ AuthBlocked "Attempts limit exceed."
  smsCfg <- asks (.smsCfg)
  let altNumber = req.alternateNumber
      counCode = req.mobileCountryCode
  otpCode <-
    Redis.get (makeAlternateNumberOtpKey personId) >>= \case
      Nothing -> do
        let fakeOtp = show <$> useFakeSms smsCfg
        newOtp <- maybe generateOTPCode return fakeOtp
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Redis.setExp (makeAlternateNumberOtpKey personId) newOtp expTime
        return newOtp
      Just a -> return a
  let otpHash = smsCfg.credConfig.otpHash
      altphoneNumber = counCode <> altNumber
      sender = smsCfg.sender
  withLogTag ("personId_" <> getId personId) $ do
    message <-
      MessageBuilder.buildSendAlternateNumberOTPMessage merchantOpCityId $
        MessageBuilder.BuildSendOTPMessageReq
          { otp = otpCode,
            hash = otpHash
          }
    Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message altphoneNumber sender)
      >>= Sms.checkSmsResult
  updAttempts <- Redis.decrby (makeAlternateNumberAttemptsKey personId) 1
  let updAttempt = fromIntegral updAttempts
  return $ ResendAuth {auth = otpCode, attemptsLeft = updAttempt}

remove ::
  ( KvDbFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CacheFlow m r
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m APISuccess
remove (personId, _, _) = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let driver =
        person
          { SP.unencryptedAlternateMobileNumber = Nothing,
            SP.alternateMobileNumber = Nothing
          }
  void $ QPerson.updateAlternateMobileNumberAndCode driver
  return Success

-- history should be on basis of invoice instead of driverFee id
getDriverPayments ::
  (EsqDBReplicaFlow m r, KvDbFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Day ->
  Maybe Day ->
  Maybe DDF.DriverFeeStatus ->
  Maybe Int ->
  Maybe Int ->
  ServiceNames ->
  m [DriverPaymentHistoryResp]
getDriverPayments (personId, _, merchantOpCityId) mbFrom mbTo mbStatus mbLimit mbOffset serviceName = do
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit -- TODO move to common code
      offset = fromMaybe 0 mbOffset
      defaultFrom = fromMaybe (fromGregorian 2020 1 1) mbFrom
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let today = utctDay now
      from = fromMaybe defaultFrom mbFrom
      to = fromMaybe today mbTo
  let windowStartTime = UTCTime from 0
      windowEndTime = addUTCTime (86399 + transporterConfig.driverPaymentCycleDuration) (UTCTime to 0)
  driverFees <- runInReplica $ QDF.findWindowsWithStatusAndServiceName personId windowStartTime windowEndTime mbStatus limit offset serviceName

  driverFeeByInvoices <- case driverFees of
    [] -> pure []
    _ -> SLDriverFee.groupDriverFeeByInvoices currency driverFees

  mapM buildPaymentHistory driverFeeByInvoices
  where
    maxLimit = 20
    defaultLimit = 10

    buildPaymentHistory SLDriverFee.DriverFeeByInvoice {..} = do
      let charges = totalFee
          chargesBreakup = mkChargesBreakup currency govtCharges platformFee.fee platformFee.cgst platformFee.sgst
          totalRides = numRides
          driverFeeId = cast invoiceId
      transactionDetails <- findAllByOrderId (cast invoiceId)
      let txnInfo = map mkDriverTxnInfo transactionDetails
      return
        DriverPaymentHistoryResp
          { totalEarnings = roundToIntegral totalEarnings,
            totalEarningsWithCurrency = PriceAPIEntity totalEarnings currency,
            charges = roundToIntegral charges,
            chargesWithCurrency = PriceAPIEntity charges currency,
            ..
          }

    mkDriverTxnInfo PaymentTransaction {..} = DriverTxnInfo {..}

    mkChargesBreakup currency govtCharges platformFee cgst sgst =
      [ DriverPaymentBreakup
          { component = "Government Charges",
            amount = govtCharges,
            amountWithCurrency = PriceAPIEntity govtCharges currency
          },
        DriverPaymentBreakup
          { component = "Platform Fee",
            amount = platformFee,
            amountWithCurrency = PriceAPIEntity platformFee currency
          },
        DriverPaymentBreakup
          { component = "CGST",
            amount = cgst,
            amountWithCurrency = PriceAPIEntity cgst currency
          },
        DriverPaymentBreakup
          { component = "SGST",
            amount = sgst,
            amountWithCurrency = PriceAPIEntity sgst currency
          }
      ]

clearDriverDues ::
  (EsqDBReplicaFlow m r, KvDbFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ServiceNames ->
  Maybe SPayment.DeepLinkData ->
  m ClearDuesRes
clearDriverDues (personId, _merchantId, opCityId) serviceName mbDeepLinkData = do
  dueDriverFees <- QDF.findAllByStatusAndDriverIdWithServiceName personId [DDF.PAYMENT_OVERDUE] serviceName
  invoices <- (runInReplica . QINV.findActiveManualInvoiceByFeeId . (.id)) `mapM` dueDriverFees
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCityId serviceName
      >>= fromMaybeM (NoSubscriptionConfigForService opCityId.getId $ show serviceName)
  let paymentService = subscriptionConfig.paymentServiceName
  let sortedInvoices = mergeSortAndRemoveDuplicate invoices
  case sortedInvoices of
    [] -> do mkClearDuesResp <$> SPayment.createOrder (personId, _merchantId, opCityId) paymentService (dueDriverFees, []) Nothing INV.MANUAL_INVOICE Nothing mbDeepLinkData
    (invoice_ : restinvoices) -> do
      mapM_ (QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE . (.id)) restinvoices
      (invoice, currentDuesForExistingInvoice, newDues) <- validateExistingInvoice invoice_ dueDriverFees
      let driverFeeForCurrentInvoice = filter (\dfee -> dfee.id.getId `elem` currentDuesForExistingInvoice) dueDriverFees
      let driverFeeToBeAddedOnExpiry = filter (\dfee -> dfee.id.getId `elem` newDues) dueDriverFees
      mkClearDuesResp <$> SPayment.createOrder (personId, _merchantId, opCityId) paymentService (driverFeeForCurrentInvoice, driverFeeToBeAddedOnExpiry) Nothing INV.MANUAL_INVOICE invoice mbDeepLinkData
  where
    validateExistingInvoice invoice driverFees = do
      invoices <- runInReplica $ QINV.findAllByInvoiceId invoice.id
      let driverFeeIds = driverFees <&> getId . (.id)
      let currentDueDriverFee = (invoices <&> getId . (.driverFeeId)) `intersect` driverFeeIds
      if length currentDueDriverFee <= length invoices
        then do
          return (Just (invoice.id, invoice.invoiceShortId), currentDueDriverFee, (driverFees <&> getId . (.id)) \\ (invoices <&> getId . (.driverFeeId)))
        else do
          QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE invoice.id
          return (Nothing, driverFeeIds, [])

    mkClearDuesResp (orderResp, orderId) = ClearDuesRes {orderId = orderId, orderResp}

    mergeSortAndRemoveDuplicate :: [[INV.Invoice]] -> [INV.Invoice]
    mergeSortAndRemoveDuplicate invoices = do
      let uniqueInvoices = DL.nubBy (\x y -> x.id == y.id) (concat invoices)
      sortOn (Down . (.createdAt)) uniqueInvoices

data HistoryEntityV2 = HistoryEntityV2
  { autoPayInvoices :: [AutoPayInvoiceHistory],
    manualPayInvoices :: [ManualInvoiceHistory]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data AutoPayInvoiceHistory = AutoPayInvoiceHistory
  { invoiceId :: Text,
    amount :: HighPrecMoney,
    amountWithCurrency :: PriceAPIEntity,
    executionAt :: UTCTime,
    autoPayStage :: Maybe DDF.AutopayPaymentStage,
    rideTakenOn :: UTCTime,
    isCoinCleared :: Bool,
    coinDiscountAmount :: Maybe HighPrecMoney,
    coinDiscountAmountWithCurrency :: Maybe PriceAPIEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ManualInvoiceHistory = ManualInvoiceHistory
  { invoiceId :: Text,
    createdAt :: UTCTime,
    rideDays :: Int,
    rideTakenOn :: Maybe UTCTime,
    amount :: HighPrecMoney,
    amountWithCurrency :: PriceAPIEntity,
    feeType :: DDF.FeeType,
    isCoinCleared :: Bool,
    coinDiscountAmount :: Maybe HighPrecMoney,
    coinDiscountAmountWithCurrency :: Maybe PriceAPIEntity,
    paymentStatus :: INV.InvoiceStatus
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

getDriverPaymentsHistoryV2 ::
  (EsqDBReplicaFlow m r, KvDbFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe INV.InvoicePaymentMode ->
  Maybe Int ->
  Maybe Int ->
  ServiceNames ->
  m HistoryEntityV2
getDriverPaymentsHistoryV2 (driverId, _, merchantOpCityId) mPaymentMode mbLimit mbOffset serviceName = do
  let defaultLimit = 20
      limit = min defaultLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
      manualInvoiceModes = [INV.MANUAL_INVOICE, INV.MANDATE_SETUP_INVOICE, INV.CASH_COLLECTED_INVOICE]
      modes = maybe manualInvoiceModes (\mode -> if mode == INV.AUTOPAY_INVOICE then [INV.AUTOPAY_INVOICE] else manualInvoiceModes) mPaymentMode
  invoices <- QINV.findAllInvoicesByDriverIdWithLimitAndOffset driverId modes limit offset serviceName
  driverFeeForInvoices <- QDF.findAllByDriverFeeIds (invoices <&> (.driverFeeId))
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId) -- check if there is error type already for this
  let mapDriverFeeByDriverFeeId = M.fromList (map (\dfee -> (dfee.id, dfee)) driverFeeForInvoices)

  (manualPayInvoices, autoPayInvoices) <-
    case mPaymentMode of
      Just INV.AUTOPAY_INVOICE -> do
        autoPayInvoices <- mapMaybeM (mkAutoPayPaymentEntity mapDriverFeeByDriverFeeId transporterConfig) invoices
        return ([], autoPayInvoices)
      _ -> do
        manualPayInvoices_ <- mapMaybeM (\manualInvoice -> mkManualPaymentEntity manualInvoice mapDriverFeeByDriverFeeId transporterConfig) invoices
        return (manualPayInvoices_, [])

  return HistoryEntityV2 {autoPayInvoices, manualPayInvoices}

mkManualPaymentEntity :: KvDbFlow m r => INV.Invoice -> Map (Id DDF.DriverFee) DDF.DriverFee -> TransporterConfig -> m (Maybe ManualInvoiceHistory)
mkManualPaymentEntity manualInvoice mapDriverFeeByDriverFeeId' transporterConfig = do
  allEntriesByInvoiceId <- QINV.findAllByInvoiceId manualInvoice.id
  allDriverFeeForInvoice <- QDF.findAllByDriverFeeIds (allEntriesByInvoiceId <&> (.driverFeeId))
  let amount = sum $ mapToAmount allDriverFeeForInvoice
  case mapDriverFeeByDriverFeeId' M.!? (manualInvoice.driverFeeId) of
    Just dfee ->
      return $
        Just
          ManualInvoiceHistory
            { invoiceId = manualInvoice.invoiceShortId,
              rideDays = length allDriverFeeForInvoice,
              rideTakenOn = if length allDriverFeeForInvoice == 1 then addUTCTime (-1 * secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) . (.createdAt) <$> listToMaybe allDriverFeeForInvoice else Nothing,
              amount,
              amountWithCurrency = PriceAPIEntity amount dfee.currency,
              createdAt = manualInvoice.createdAt,
              feeType = if any (\dfee' -> dfee'.feeType == DDF.MANDATE_REGISTRATION) allDriverFeeForInvoice then DDF.MANDATE_REGISTRATION else DDF.RECURRING_INVOICE,
              paymentStatus = manualInvoice.invoiceStatus,
              isCoinCleared = dfee.status == DDF.CLEARED_BY_YATRI_COINS,
              coinDiscountAmount = dfee.amountPaidByCoin,
              coinDiscountAmountWithCurrency = flip PriceAPIEntity dfee.currency <$> dfee.amountPaidByCoin
            }
    Nothing -> return Nothing
  where
    mapToAmount = map (\dueDfee -> SLDriverFee.roundToHalf dueDfee.currency (dueDfee.govtCharges + dueDfee.platformFee.fee + dueDfee.platformFee.cgst + dueDfee.platformFee.sgst))

mkAutoPayPaymentEntity :: MonadFlow m => Map (Id DDF.DriverFee) DDF.DriverFee -> TransporterConfig -> INV.Invoice -> m (Maybe AutoPayInvoiceHistory)
mkAutoPayPaymentEntity mapDriverFeeByDriverFeeId' transporterConfig autoInvoice = do
  now <- getCurrentTime
  case mapDriverFeeByDriverFeeId' M.!? (autoInvoice.driverFeeId) of
    Just dfee ->
      let executionTime =
            if dfee.status == DDF.CLEARED_BY_YATRI_COINS
              then autoInvoice.createdAt
              else maybe now (DAPlan.calcExecutionTime transporterConfig dfee.autopayPaymentStage) dfee.stageUpdatedAt
       in return $
            Just
              AutoPayInvoiceHistory
                { invoiceId = autoInvoice.invoiceShortId,
                  amount = sum $ mapToAmount [dfee],
                  amountWithCurrency = PriceAPIEntity (sum $ mapToAmount [dfee]) dfee.currency,
                  executionAt = executionTime,
                  autoPayStage = dfee.autopayPaymentStage,
                  rideTakenOn = addUTCTime (-1 * secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) dfee.createdAt,
                  isCoinCleared = dfee.status == DDF.CLEARED_BY_YATRI_COINS,
                  coinDiscountAmount = dfee.amountPaidByCoin,
                  coinDiscountAmountWithCurrency = flip PriceAPIEntity dfee.currency <$> (dfee.amountPaidByCoin)
                }
    Nothing -> return Nothing
  where
    mapToAmount = map (\dueDfee -> SLDriverFee.roundToHalf dueDfee.currency (dueDfee.govtCharges + dueDfee.platformFee.fee + dueDfee.platformFee.cgst + dueDfee.platformFee.sgst))

data HistoryEntryDetailsEntityV2 = HistoryEntryDetailsEntityV2
  { invoiceId :: Text,
    amount :: HighPrecMoney,
    amountWithCurrency :: PriceAPIEntity,
    createdAt :: Maybe UTCTime,
    executionAt :: Maybe UTCTime,
    feeType :: DDF.FeeType,
    driverFeeInfo :: [DriverFeeInfoEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data DriverFeeInfoEntity = DriverFeeInfoEntity
  { autoPayStage :: Maybe DDF.AutopayPaymentStage,
    paymentStatus :: Maybe INV.InvoiceStatus,
    totalEarnings :: HighPrecMoney,
    totalEarningsWithCurrency :: PriceAPIEntity,
    totalRides :: Int,
    planAmount :: HighPrecMoney,
    planAmountWithCurrency :: PriceAPIEntity,
    rideTakenOn :: UTCTime,
    driverFeeAmount :: HighPrecMoney,
    driverFeeAmountWithCurrency :: PriceAPIEntity,
    maxRidesEligibleForCharge :: Maybe Int,
    isSplit :: Bool,
    offerAndPlanDetails :: Maybe Text,
    isCoinCleared :: Bool,
    coinDiscountAmount :: Maybe HighPrecMoney,
    coinDiscountAmountWithCurrency :: Maybe PriceAPIEntity,
    specialZoneRideCount :: Int,
    totalSpecialZoneCharges :: HighPrecMoney,
    totalSpecialZoneChargesWithCurrency :: PriceAPIEntity,
    vehicleNumber :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

getHistoryEntryDetailsEntityV2 ::
  (EsqDBReplicaFlow m r, KvDbFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Text ->
  ServiceNames ->
  m HistoryEntryDetailsEntityV2
getHistoryEntryDetailsEntityV2 (driverId, _, merchantOpCityId) invoiceShortId serviceName = do
  allEntiresByInvoiceId <- QINV.findAllByInvoiceShortId invoiceShortId
  allDriverFeeForInvoice <- QDF.findAllByDriverFeeIds (allEntiresByInvoiceId <&> (.driverFeeId))
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getCurrentTime
  let amount = sum $ mapToAmount allDriverFeeForInvoice
      invoiceType = listToMaybe allEntiresByInvoiceId <&> (.paymentMode)
      createdAt = if invoiceType `elem` [Just INV.MANUAL_INVOICE, Just INV.MANDATE_SETUP_INVOICE, Nothing] then listToMaybe allEntiresByInvoiceId <&> (.createdAt) else Nothing
      mbAutoPayStage = listToMaybe allDriverFeeForInvoice >>= (.autopayPaymentStage)
      mbStageUpdatedAt = listToMaybe allDriverFeeForInvoice >>= (.stageUpdatedAt)
      executionAt =
        if invoiceType == Just INV.AUTOPAY_INVOICE
          then
            if ((.invoiceStatus) <$> listToMaybe allEntiresByInvoiceId) == Just INV.CLEARED_BY_YATRI_COINS
              then (.createdAt) <$> listToMaybe allEntiresByInvoiceId
              else Just $ maybe now (DAPlan.calcExecutionTime transporterConfig mbAutoPayStage) mbStageUpdatedAt
          else Nothing
      feeType
        | any (\dfee -> dfee.feeType == DDF.MANDATE_REGISTRATION) allDriverFeeForInvoice = DDF.MANDATE_REGISTRATION
        | invoiceType == Just INV.AUTOPAY_INVOICE = DDF.RECURRING_EXECUTION_INVOICE
        | otherwise = DDF.RECURRING_INVOICE
  currency <- case allDriverFeeForInvoice of
    [] -> SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
    (fee : _) -> pure fee.currency

  driverFeeInfo' <- mkDriverFeeInfoEntity allDriverFeeForInvoice (listToMaybe allEntiresByInvoiceId <&> (.invoiceStatus)) transporterConfig serviceName
  return $ HistoryEntryDetailsEntityV2 {invoiceId = invoiceShortId, amount, amountWithCurrency = PriceAPIEntity amount currency, createdAt, executionAt, feeType, driverFeeInfo = driverFeeInfo'}
  where
    mapToAmount = map (\dueDfee -> SLDriverFee.roundToHalf dueDfee.currency (dueDfee.govtCharges + dueDfee.platformFee.fee + dueDfee.platformFee.cgst + dueDfee.platformFee.sgst))

mkDriverFeeInfoEntity ::
  KvDbFlow m r =>
  [DDF.DriverFee] ->
  Maybe INV.InvoiceStatus ->
  TransporterConfig ->
  ServiceNames ->
  m [DriverFeeInfoEntity]
mkDriverFeeInfoEntity driverFees invoiceStatus transporterConfig serviceName = do
  mapM
    ( \driverFee -> do
        driverFeesInWindow <- QDF.findFeeInRangeAndDriverIdAndServiceName driverFee.startTime driverFee.endTime driverFee.driverId serviceName
        mbPlan <- DAPlan.getPlanDataFromDriverFee driverFee
        let maxRidesEligibleForCharge = DAPlan.planMaxRides =<< mbPlan
        return
          DriverFeeInfoEntity
            { autoPayStage = driverFee.autopayPaymentStage,
              paymentStatus = invoiceStatus,
              totalEarnings = driverFee.totalEarnings,
              totalEarningsWithCurrency = PriceAPIEntity driverFee.totalEarnings driverFee.currency,
              driverFeeAmount = (\dueDfee -> SLDriverFee.roundToHalf dueDfee.currency (dueDfee.govtCharges + dueDfee.platformFee.fee + dueDfee.platformFee.cgst + dueDfee.platformFee.sgst)) driverFee,
              driverFeeAmountWithCurrency = PriceAPIEntity (SLDriverFee.roundToHalf driverFee.currency (driverFee.govtCharges + driverFee.platformFee.fee + driverFee.platformFee.cgst + driverFee.platformFee.sgst)) driverFee.currency,
              totalRides = SLDriverFee.calcNumRides driverFee transporterConfig,
              planAmount = fromMaybe 0 driverFee.feeWithoutDiscount,
              planAmountWithCurrency = PriceAPIEntity (fromMaybe 0 driverFee.feeWithoutDiscount) driverFee.currency,
              isSplit = length driverFeesInWindow > 1,
              rideTakenOn = addUTCTime (-1 * secondsToNominalDiffTime transporterConfig.timeDiffFromUtc) driverFee.createdAt, --- when we fix ist issue we will remove this,
              offerAndPlanDetails = driverFee.planOfferTitle,
              isCoinCleared = driverFee.status == DDF.CLEARED_BY_YATRI_COINS,
              coinDiscountAmount = driverFee.amountPaidByCoin,
              coinDiscountAmountWithCurrency = flip PriceAPIEntity driverFee.currency <$> driverFee.amountPaidByCoin,
              specialZoneRideCount = driverFee.specialZoneRideCount,
              totalSpecialZoneCharges = driverFee.specialZoneAmount,
              totalSpecialZoneChargesWithCurrency = flip PriceAPIEntity driverFee.currency driverFee.specialZoneAmount,
              vehicleNumber = driverFee.vehicleNumber,
              maxRidesEligibleForCharge
            }
    )
    driverFees

getCity :: KvDbFlow m r => GetCityReq -> m GetCityResp
getCity req = do
  let latlng = LatLong {lat = req.lat, lon = req.lon}
  geometry <-
    runInReplica $
      QGeometry.findGeometriesContainingGps latlng >>= \case
        [] -> do
          pure Nothing
        (g : _) -> pure $ Just g
  let city = (.city) <$> geometry
  pure $ GetCityResp {city = show <$> city, status = APISuccess.Success}

data DriverFeeResp = DriverFeeResp
  { createdAt :: UTCTime, -- window start day
    driverFeeId :: Id DDF.DriverFee,
    chargesBreakup :: [DriverPaymentBreakup],
    status :: DDF.DriverFeeStatus,
    totalRides :: Int,
    planOfferTitle :: Maybe Text,
    billNumber :: Maybe Int,
    debitedOn :: Maybe UTCTime,
    amountPaidByCoin :: Maybe HighPrecMoney,
    feeWithoutDiscount :: Maybe HighPrecMoney,
    amountPaidByCoinWithCurrency :: Maybe PriceAPIEntity,
    feeWithoutDiscountWithCurrency :: Maybe PriceAPIEntity,
    invoiceStatus :: Maybe INV.InvoiceStatus
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

getDownloadInvoiceData :: (EsqDBReplicaFlow m r, KvDbFlow m r, EncFlow m r, CacheFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Day -> Maybe Day -> m [DriverFeeResp]
getDownloadInvoiceData (personId, _merchantId, merchantOpCityId) from mbTo = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let today = utctDay now
      to = fromMaybe today mbTo
  let windowStartTime = UTCTime from 0
      windowEndTime = addUTCTime transporterConfig.driverPaymentCycleDuration (UTCTime to 0)
  driverFees <- runInReplica $ QDF.findWindowsWithoutLimit personId windowStartTime windowEndTime
  mapM buildDriverFeeRespEntity driverFees
  where
    buildDriverFeeRespEntity DDF.DriverFee {..} = do
      mbInvoice <- QINV.findLatestByDriverFeeId id
      return
        DriverFeeResp
          { chargesBreakup = mkChargesBreakup currency govtCharges platformFee.fee platformFee.cgst platformFee.sgst,
            totalRides = numRides,
            driverFeeId = id,
            debitedOn = mbInvoice <&> (.updatedAt),
            invoiceStatus = mbInvoice <&> (.invoiceStatus),
            amountPaidByCoinWithCurrency = flip PriceAPIEntity currency <$> amountPaidByCoin,
            feeWithoutDiscountWithCurrency = flip PriceAPIEntity currency <$> feeWithoutDiscount,
            ..
          }

    mkChargesBreakup currency _ platformFee cgst sgst =
      [ DriverPaymentBreakup
          { component = "Final Platform Fee",
            amount = platformFee + cgst + sgst,
            amountWithCurrency = PriceAPIEntity (platformFee + cgst + sgst) currency
          },
        DriverPaymentBreakup
          { component = "Platform Fee",
            amount = platformFee,
            amountWithCurrency = PriceAPIEntity platformFee currency
          },
        DriverPaymentBreakup
          { component = "CGST",
            amount = cgst,
            amountWithCurrency = PriceAPIEntity cgst currency
          },
        DriverPaymentBreakup
          { component = "SGST",
            amount = sgst,
            amountWithCurrency = PriceAPIEntity sgst currency
          }
      ]

getDummyRideRequest ::
  ( EsqDBReplicaFlow m r,
    KvDbFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m APISuccess
getDummyRideRequest (personId, _, merchantOpCityId) = do
  driver <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  DriverNotify.triggerDummyRideRequest driver merchantOpCityId False
