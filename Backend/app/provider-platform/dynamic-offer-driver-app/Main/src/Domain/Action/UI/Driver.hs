{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
    ScheduledBookingRes (..),
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
    UpdateProfileInfoPoints (..),
    RefundByPayoutReq (..),
    SecurityDepositDfStatusRes (..),
    ClearManualSelectedDues (..),
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
    driverProfileImagesUpload,
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
    listScheduledBookings,
    acceptScheduledBooking,
    getInformationV2,
    clearDriverFeeWithCreate,
    verifyVpaStatus,
    getSecurityDepositDfStatus,
    refundByPayoutDriverFee,
    mkPayoutLockKeyByDriverAndService,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Message as Common
import qualified API.Types.UI.DriverOnboardingV2 as DOVT
import API.UI.Issue (driverIssueHandle)
import AWS.S3 as S3
import Control.Monad.Extra (mapMaybeM)
import qualified Data.Aeson as DA
import qualified Data.Aeson.KeyMap as DAKM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Digest.Pure.MD5 as MD5
import Data.Either.Extra (eitherToMaybe)
import Data.List (intersect, nub, (\\))
import qualified Data.List as DL
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (defaultTimeLocale, parseTimeM)
import qualified Data.Tuple.Extra as DTE
import Domain.Action.Beckn.Search
import Domain.Action.Dashboard.Driver.Notification as DriverNotify (triggerDummyRideRequest)
import qualified Domain.Action.UI.DriverGoHomeRequest as DDGR
import qualified Domain.Action.UI.DriverHomeLocation as DDHL
import Domain.Action.UI.DriverOnboarding.AadhaarVerification (fetchAndCacheAadhaarImage)
import qualified Domain.Action.UI.DriverOnboardingV2 as DOV
import qualified Domain.Action.UI.Merchant as DM
import qualified Domain.Action.UI.Payout as Payout
import qualified Domain.Action.UI.Person as SP
import qualified Domain.Action.UI.Plan as DAPlan
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.Booking as DRB
import Domain.Types.Common
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DriverBankAccount as DOBA
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import qualified Domain.Types.DriverHomeLocation as DDHL
import Domain.Types.DriverInformation (DriverInformation)
import qualified Domain.Types.DriverInformation as DriverInfo
import qualified Domain.Types.DriverPlan as DPlan
import qualified Domain.Types.DriverQuote as DDrQuote
import qualified Domain.Types.DriverReferral as DR
import Domain.Types.DriverStats
import qualified Domain.Types.DriverStats as DStats
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as Fare
import Domain.Types.FarePolicy (DriverExtraFeeBounds (..))
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.Invoice as Domain
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DTM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import qualified Domain.Types.Person as SP
import Domain.Types.Plan as Plan
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchRequestForDriver as DSRD
import qualified Domain.Types.SearchTry as DST
import Domain.Types.TransporterConfig
import Domain.Types.Vehicle (Vehicle (..), VehicleAPIEntity)
import Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleCategory as DVC
import Domain.Types.VehicleVariant
import qualified Domain.Types.VehicleVariant as DV
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (decodeUtf8, id, state)
import qualified EulerHS.Prelude as Prelude
import GHC.Records.Extra
import qualified IssueManagement.Common.UI.Issue as Issue
import qualified IssueManagement.Domain.Action.UI.Issue as Issue
import qualified IssueManagement.Domain.Types.MediaFile as Domain
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import Kernel.Beam.Functions
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Notification.FCM.Types (FCMRecipientToken)
import Kernel.External.Payment.Interface
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Kernel.External.Payout.Interface as Juspay
import qualified Kernel.External.Payout.Types as TPayout
import qualified Kernel.External.Verification.Interface.InternalScripts as IF
import Kernel.Prelude (NominalDiffTime, handle, intToNominalDiffTime, roundToIntegral)
import Kernel.Serviceability (rideServiceable)
import Kernel.Sms.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version
import Kernel.Utils.CalculateDistance
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Validation
import Kernel.Utils.Version
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Lib.Payment.Domain.Types.PaymentTransaction
import Lib.Payment.Storage.Queries.PaymentTransaction
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Lib.Types.SpecialLocation as SL
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.Allocator (AllocatorJobType (..), ScheduledRideAssignedOnUpdateJobData (..))
import qualified SharedLogic.BehaviourManagement.CancellationRate as SCR
import SharedLogic.Booking
import SharedLogic.Cac
import SharedLogic.CallBAP (sendDriverOffer, sendRideAssignedUpdateToBAP)
import qualified SharedLogic.DeleteDriver as DeleteDriverOnCheck
import qualified SharedLogic.DriverFee as SLDriverFee
import SharedLogic.DriverOnboarding
import SharedLogic.DriverPool as DP
import SharedLogic.DriverPool as SDP
import qualified SharedLogic.EventTracking as ET
import qualified SharedLogic.External.LocationTrackingService.Flow as LTF
import SharedLogic.FareCalculator
import SharedLogic.FarePolicy
import qualified SharedLogic.Merchant as SMerchant
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.Payment as SPayment
import SharedLogic.Ride
import qualified SharedLogic.SearchTryLocker as CS
import SharedLogic.VehicleServiceTier
import qualified Storage.Cac.DriverPoolConfig as SCDPC
import qualified Storage.Cac.GoHomeConfig as CGHC
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.BapMetadata as CQSM
import Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.FareProduct as CQFP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DailyStats as SQDS
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverFeeExtra as QDFE
import qualified Storage.Queries.DriverGoHomeRequest as QDGR
import qualified Storage.Queries.DriverHomeLocation as QDHL
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverPlan as QDriverPlan
import qualified Storage.Queries.DriverQuote as QDrQt
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDriverStats
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
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VendorFee as QVF
import qualified Tools.Auth as Auth
import Tools.Error
import Tools.Event
import qualified Tools.Payout as Payout
import Tools.SMS as Sms hiding (Success)
import Tools.Verification hiding (length)
import Utils.Common.Cac.KeyNameConstants

data DriverInformationRes = DriverInformationRes
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    operatingCity :: Context.City,
    numberOfRides :: Int,
    mobileNumber :: Maybe Text,
    email :: Maybe Text,
    linkedVehicle :: Maybe VehicleAPIEntity,
    rating :: Maybe Centesimal,
    active :: Bool,
    onRide :: Bool,
    verified :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    blockExpiryTime :: Maybe UTCTime,
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
    canSwitchToIntraCity :: Bool,
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
    frontendConfigHash :: Maybe Text,
    bankDetails :: Maybe DOVT.BankAccountResp,
    payoutVpa :: Maybe Text,
    payoutVpaStatus :: Maybe DriverInfo.PayoutVpaStatus,
    isPayoutEnabled :: Maybe Bool,
    payoutRewardAmount :: Maybe HighPrecMoney,
    payoutVpaBankAccount :: Maybe Text,
    cancellationRateInWindow :: Maybe Int,
    cancelledRidesCountInWindow :: Maybe Int,
    assignedRidesCountInWindow :: Maybe Int,
    windowSize :: Maybe Int,
    isSubscriptionVehicleCategoryChanged :: Bool,
    isOnFreeTrial :: Bool,
    planMandatoryForCategory :: Bool,
    isSubscriptionCityChanged :: Bool,
    freeTrialDays :: Int,
    freeTrialRides :: Int,
    totalRidesTaken :: Maybe Int,
    favCount :: Maybe Int,
    subscriptionEnabledForVehicleCategory :: Bool,
    isSubscriptionEnabledAtCategoryLevel :: Bool,
    isSpecialLocWarrior :: Bool,
    safetyTag :: Maybe DA.Value,
    safetyScore :: Maybe DA.Value,
    overchargingTag :: Maybe DA.Value,
    ridesWithFareIssues :: Maybe DA.Value,
    totalRidesConsideredForFareIssues :: Maybe DA.Value,
    blockedReasonFlag :: Maybe BlockReasonFlag,
    softBlockStiers :: Maybe [ServiceTierType],
    softBlockExpiryTime :: Maybe UTCTime,
    softBlockReasonFlag :: Maybe Text,
    onboardingVehicleCategory :: Maybe VehicleCategory,
    subscriptionDown :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DriverEntityRes = DriverEntityRes
  { id :: Id Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    email :: Maybe Text,
    linkedVehicle :: Maybe VehicleAPIEntity,
    rating :: Maybe Centesimal,
    active :: Bool,
    onRide :: Bool,
    enabled :: Bool,
    blocked :: Bool,
    blockExpiryTime :: Maybe UTCTime,
    blockedReasonFlag :: Maybe BlockReasonFlag,
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
    canSwitchToIntraCity :: Bool,
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
    isVehicleSupported :: Bool,
    payoutVpa :: Maybe Text,
    payoutVpaStatus :: Maybe DriverInfo.PayoutVpaStatus,
    payoutVpaBankAccount :: Maybe Text,
    isSubscriptionVehicleCategoryChanged :: Bool,
    isOnFreeTrial :: Bool,
    planMandatoryForCategory :: Bool,
    isSubscriptionCityChanged :: Bool,
    freeTrialDays :: Int,
    freeTrialRides :: Int,
    totalRidesTaken :: Maybe Int,
    subscriptionEnabledForVehicleCategory :: Bool,
    isSubscriptionEnabledAtCategoryLevel :: Bool,
    isSpecialLocWarrior :: Bool,
    safetyTag :: Maybe DA.Value,
    safetyScore :: Maybe DA.Value,
    overchargingTag :: Maybe DA.Value,
    ridesWithFareIssues :: Maybe DA.Value,
    totalRidesConsideredForFareIssues :: Maybe DA.Value,
    softBlockStiers :: Maybe [ServiceTierType],
    softBlockExpiryTime :: Maybe UTCTime,
    softBlockReasonFlag :: Maybe Text,
    onboardingVehicleCategory :: Maybe VehicleCategory,
    subscriptionDown :: Maybe Bool
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
    canSwitchToIntraCity :: Maybe Bool,
    clientVersion :: Maybe Version,
    bundleVersion :: Maybe Version,
    gender :: Maybe SP.Gender,
    languagesSpoken :: Maybe [Text],
    hometown :: Maybe Text,
    vehicleName :: Maybe Text,
    availableUpiApps :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype ScheduledBookingRes = ScheduledBookingRes
  { bookings :: [ScheduleBooking]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data ScheduleBooking = ScheduleBooking
  { bookingDetails :: BookingAPIEntity,
    fareDetails :: [DOVT.RateCardItem]
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data BookingAPIEntity = BookingAPIEntity
  { id :: Id DRB.Booking,
    status :: DRB.BookingStatus,
    tripCategory :: DTC.TripCategory,
    specialZoneOtpCode :: Maybe Text,
    disabilityTag :: Maybe Text,
    area :: Maybe SL.Area,
    startTime :: UTCTime,
    fromLocation :: DLoc.Location,
    toLocation :: Maybe DLoc.Location,
    stops :: [DLoc.Location],
    vehicleServiceTier :: DVST.ServiceTierType,
    vehicleServiceTierName :: Text,
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    vehicleServiceTierAirConditioned :: Maybe Double,
    isAirConditioned :: Maybe Bool,
    estimatedDistance :: Maybe Meters,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    estimatedFare :: HighPrecMoney,
    currency :: Currency,
    estimatedDuration :: Maybe Seconds,
    fareParams :: FareParameters,
    tollNames :: Maybe [Text],
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    stopLocationId :: Maybe (Id DLoc.Location),
    roundTrip :: Maybe Bool,
    returnTime :: Maybe UTCTime,
    distanceToPickup :: Maybe Meters,
    isScheduled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdateProfileInfoPoints = UpdateProfileInfoPoints
  { isAdvancedBookingEnabled :: Maybe Bool,
    isInteroperable :: Maybe Bool,
    isCategoryLevelSubscriptionEnabled :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- data Stop = Stop
--   { location :: DLoc.Location,
--     stopInfo :: Maybe DSI.StopInformation
--   }
--   deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

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
    response :: SearchRequestForDriverResponse,
    notificationSource :: Maybe NotificationSource,
    renderedAt :: Maybe UTCTime,
    respondedAt :: Maybe UTCTime
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
    totalValidRidesOfDay :: Int,
    tipsEarning :: PriceAPIEntity
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
    lon :: Double,
    merchantId :: Maybe (Id DM.Merchant)
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data GetCityResp = GetCityResp
  { city :: Maybe Text,
    status :: APISuccess
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ClearManualSelectedDues = ClearManualSelectedDues
  { driverFeeIds :: [Id DDF.DriverFee]
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON, Show, Ord, Eq)

getInformationV2 ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  UpdateProfileInfoPoints ->
  m DriverInformationRes
getInformationV2 (personId, merchantId, merchantOpCityId) mbClientId toss tenant' context req = do
  whenJust req.isCategoryLevelSubscriptionEnabled $ \isCategoryLevelSubscriptionEnabled ->
    QDriverPlan.updateIsSubscriptionEnabledAtCategoryLevel personId YATRI_SUBSCRIPTION isCategoryLevelSubscriptionEnabled
  driverInfo <- QDriverInformation.findById personId >>= fromMaybeM DriverInfoNotFound
  when (req.isAdvancedBookingEnabled /= Just driverInfo.forwardBatchingEnabled || req.isInteroperable /= Just driverInfo.isInteroperable) $
    QDriverInformation.updateForwardBatchingEnabledOrIsInteroperable personId req.isAdvancedBookingEnabled req.isInteroperable
  getInformation (personId, merchantId, merchantOpCityId) mbClientId toss tenant' context (Just driverInfo)

getInformation ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe DriverInformation ->
  m DriverInformationRes
getInformation (personId, merchantId, merchantOpCityId) mbClientId toss tnant' context mbDriverInfo = do
  let driverId = cast personId
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  when (isNothing person.clientId && isJust mbClientId) $ QPerson.updateClientId mbClientId person.id
  driverStats <- runInReplica $ QDriverStats.findById driverId >>= fromMaybeM DriverInfoNotFound
  driverInfo <- maybe (QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound) return mbDriverInfo
  driverReferralCode <- fmap (.referralCode) <$> QDR.findById (cast driverId)
  driverEntity <- buildDriverEntityRes (person, driverInfo, driverStats, merchantOpCityId)
  dues <- QDF.findAllPendingAndDueDriverFeeByDriverIdForServiceName driverId YATRI_SUBSCRIPTION
  let currentDues = sum $ map (\dueInvoice -> SLDriverFee.roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) dues
  let manualDues = sum $ map (\dueInvoice -> SLDriverFee.roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) $ filter (\due -> due.status == DDF.PAYMENT_OVERDUE) dues
  logDebug $ "alternateNumber-" <> show driverEntity.alternateNumber
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (.useCACForFrontend) systemConfigs
  let context' = fromMaybe DAKM.empty (DA.decode $ BSL.pack $ T.unpack $ fromMaybe "{}" context)
  frntndfgs <- if useCACConfig then getFrontendConfigs merchantOpCityId toss tnant' context' else return Nothing
  let mbMd5Digest = T.pack . show . MD5.md5 . DA.encode <$> frntndfgs
  merchant <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  driverGoHomeInfo <- CQDGR.getDriverGoHomeRequestInfo driverId merchantOpCityId Nothing
  makeDriverInformationRes merchantOpCityId driverEntity merchant driverReferralCode driverStats driverGoHomeInfo (Just currentDues) (Just manualDues) mbMd5Digest

setActivity :: (CacheFlow m r, EsqDBFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Bool -> Maybe DriverInfo.DriverMode -> m APISuccess.APISuccess
setActivity (personId, merchantId, merchantOpCityId) isActive mode = do
  void $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let driverId = cast personId
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  when (isActive || (isJust mode && (mode == Just DriverInfo.SILENT || mode == Just DriverInfo.ONLINE))) $ do
    merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
    transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
    mbVehicle <- QVehicle.findById personId
    DriverSpecificSubscriptionData {..} <- getDriverSpecificSubscriptionDataWithSubsConfig (personId, merchantId, merchantOpCityId) transporterConfig driverInfo mbVehicle
    let commonSubscriptionChecks = not isOnFreeTrial && not transporterConfig.allowDefaultPlanAllocation
    (planBasedChecks, changeBasedChecks) <- do
      if isSubscriptionEnabledAtCategoryLevel
        then do
          let planBasedChecks' = planMandatoryForCategory && isNothing autoPayStatus && commonSubscriptionChecks && isEnabledForCategory
          let isSubscriptionEnabledAtCategoryLevelUI = (mbDriverPlan >>= (.isCategoryLevelSubscriptionEnabled)) == Just True
          let changeBasedChecks' = (isSubscriptionVehicleCategoryChanged || isSubscriptionCityChanged) && commonSubscriptionChecks && isEnabledForCategory && isSubscriptionEnabledAtCategoryLevelUI
          pure (planBasedChecks', changeBasedChecks')
        else do
          let isEnableForVariant = maybe False (`elem` transporterConfig.variantsToEnableForSubscription) (mbVehicle <&> (.variant))
          let planBasedChecks' = transporterConfig.isPlanMandatory && isNothing autoPayStatus && commonSubscriptionChecks && isEnableForVariant
          pure (planBasedChecks', False)
    when (planBasedChecks || changeBasedChecks) $ throwError (NoPlanSelected personId.getId)
    when merchant.onlinePayment $ do
      driverBankAccount <- QDBA.findByPrimaryKey driverId >>= fromMaybeM (DriverBankAccountNotFound driverId.getId)
      unless driverBankAccount.chargesEnabled $ throwError (DriverChargesDisabled driverId.getId)
    unless (driverInfo.enabled) $ throwError DriverAccountDisabled
    unless (driverInfo.subscribed || transporterConfig.openMarketUnBlocked) $ throwError DriverUnsubscribed
    when driverInfo.blocked $ do
      case driverInfo.blockExpiryTime of
        Just expiryTime -> do
          now <- getCurrentTime
          if now > expiryTime
            then do
              QDriverInformation.updateBlockedState driverId False (Just "AUTOMATICALLY_UNBLOCKED") merchantId merchantOpCityId DTDBT.Application
            else throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)
        Nothing -> throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)
  when (driverInfo.active /= isActive || driverInfo.mode /= mode) $ QDriverInformation.updateActivity isActive (mode <|> Just DriverInfo.OFFLINE) driverId
  pure APISuccess.Success

activateGoHomeFeature :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id DDHL.DriverHomeLocation -> LatLong -> Flow APISuccess.APISuccess
activateGoHomeFeature (driverId, merchantId, merchantOpCityId) driverHomeLocationId driverLocation = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  goHomeConfig <- CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))
  unless (goHomeConfig.enableGoHome) $ throwError GoHomeFeaturePermanentlyDisabled
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  unless driverInfo.enabled $ throwError DriverAccountDisabled
  when (driverInfo.blocked) $ throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)
  let currPos = LatLong {lat = driverLocation.lat, lon = driverLocation.lon}
  driverHomeLocation <- QDHL.findById driverHomeLocationId >>= fromMaybeM (DriverHomeLocationDoesNotExist driverHomeLocationId.getId)
  when (driverHomeLocation.driverId /= driverId) $ throwError DriverHomeLocationDoesNotBelongToDriver
  let homePos = LatLong {lat = driverHomeLocation.lat, lon = driverHomeLocation.lon}
  unless (distanceBetweenInMeters homePos currPos > fromIntegral goHomeConfig.destRadiusMeters) $ throwError DriverCloseToHomeLocation
  dghInfo <- CQDGR.getDriverGoHomeRequestInfo driverId merchantOpCityId (Just goHomeConfig)
  whenM (fmap ((dghInfo.status == Just DDGR.ACTIVE) ||) (isJust <$> QDGR.findActive driverId)) $ throwError DriverGoHomeRequestAlreadyActive
  unless (dghInfo.cnt > 0) $ throwError DriverGoHomeRequestDailyUsageLimitReached
  unlessM (checkIfGoToInDifferentGeometry merchant driverLocation homePos) $ throwError CannotEnableGoHomeForDifferentCity
  activateDriverGoHomeRequest merchantId merchantOpCityId driverId driverHomeLocation goHomeConfig dghInfo
  pure APISuccess.Success
  where
    checkIfGoToInDifferentGeometry :: DM.Merchant -> LatLong -> LatLong -> Flow Bool
    checkIfGoToInDifferentGeometry merchant driverLoc = uncurry (liftM2 (\dl hl -> dl == hl && dl /= Context.AnyCity && hl /= Context.AnyCity)) . DTE.both ((((.city) . (.nearestOperatingCity)) <$>) . runInReplica . getNearestOperatingAndSourceCity merchant) . (driverLoc,)

deactivateGoHomeFeature :: (CacheFlow m r, EsqDBFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> m APISuccess.APISuccess
deactivateGoHomeFeature (personId, _, merchantOpCityId) = do
  goHomeConfig <- CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId)))
  unless (goHomeConfig.enableGoHome) $ throwError GoHomeFeaturePermanentlyDisabled
  let driverId = cast personId
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  unless driverInfo.enabled $ throwError DriverAccountDisabled
  when (driverInfo.blocked) $ throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)
  ghInfo <- getDriverGoHomeRequestInfo driverId merchantOpCityId (Just goHomeConfig)
  ghrId <- fromMaybeM DriverGoHomeRequestNotPresent ghInfo.driverGoHomeRequestId
  succRide <- Ride.findCompletedRideByGHRId ghrId
  if isJust succRide
    then CQDGR.deactivateDriverGoHomeRequest merchantOpCityId driverId DDGR.SUCCESS ghInfo (Just False)
    else CQDGR.deactivateDriverGoHomeRequest merchantOpCityId driverId DDGR.FAILED ghInfo Nothing
  pure APISuccess.Success

addHomeLocation :: (CacheFlow m r, EsqDBFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> AddHomeLocationReq -> m APISuccess.APISuccess
addHomeLocation (driverId, merchantId, merchantOpCityId) req = do
  cfg <- CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))
  unless (cfg.enableGoHome) $ throwError GoHomeFeaturePermanentlyDisabled
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  unless driverInfo.enabled $ throwError DriverAccountDisabled
  when (driverInfo.blocked) $ throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unlessM (rideServiceable merchant.geofencingConfig QGeometry.someGeometriesContain req.position Nothing) $ throwError DriverHomeLocationOutsideServiceArea
  oldHomeLocations <- QDHL.findAllByDriverId driverId
  unless (length oldHomeLocations < cfg.numHomeLocations) $ throwError DriverHomeLocationLimitReached
  when (any (\homeLocation -> highPrecMetersToMeters (distanceBetweenInMeters req.position (LatLong {lat = homeLocation.lat, lon = homeLocation.lon})) <= cfg.newLocAllowedRadius) oldHomeLocations) $ throwError NewLocationTooCloseToPreviousHomeLocation
  QDHL.create =<< buildDriverHomeLocation driverId req
  pure APISuccess.Success

buildDriverHomeLocation :: (CacheFlow m r, EsqDBFlow m r) => Id SP.Person -> AddHomeLocationReq -> m DDHL.DriverHomeLocation
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

updateHomeLocation :: (CacheFlow m r, EsqDBFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id DDHL.DriverHomeLocation -> UpdateHomeLocationReq -> m APISuccess.APISuccess
updateHomeLocation (driverId, merchantId, merchantOpCityId) homeLocationId req = do
  goHomeConfig <- CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))
  unless (goHomeConfig.enableGoHome) $ throwError GoHomeFeaturePermanentlyDisabled
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  unless driverInfo.enabled $ throwError DriverAccountDisabled
  unless (not driverInfo.blocked) $ throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)
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

getHomeLocations :: (CacheFlow m r, EsqDBFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> m GetHomeLocationsRes
getHomeLocations (driverId, _, _) = do
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  unless driverInfo.enabled $ throwError DriverAccountDisabled
  unless (not driverInfo.blocked) $ throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)
  driverHomeLocations <- QDHL.findAllByDriverId driverId
  return . GetHomeLocationsRes $ DDHL.makeDriverHomeLocationAPIEntity <$> driverHomeLocations

deleteHomeLocation :: (CacheFlow m r, EsqDBFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id DDHL.DriverHomeLocation -> m APISuccess.APISuccess
deleteHomeLocation (driverId, _, merchantOpCityId) driverHomeLocationId = do
  goHomeConfig <- CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId)))
  unless (goHomeConfig.enableGoHome) $ throwError GoHomeFeaturePermanentlyDisabled
  driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
  unless driverInfo.enabled $ throwError DriverAccountDisabled
  unless (not driverInfo.blocked) $ throwError $ DriverAccountBlocked (BlockErrorPayload driverInfo.blockExpiryTime driverInfo.blockReasonFlag)
  dghInfo <- CQDGR.getDriverGoHomeRequestInfo driverId merchantOpCityId (Just goHomeConfig)
  when (dghInfo.status == Just DDGR.ACTIVE) $ throwError DriverHomeLocationDeleteWhileActiveError
  QDHL.deleteById driverHomeLocationId
  return APISuccess.Success

buildDriverEntityRes :: (EsqDBReplicaFlow m r, EncFlow m r, CacheFlow m r, HasField "s3Env" r (S3.S3Env m), EsqDBFlow m r) => (SP.Person, DriverInformation, DStats.DriverStats, Id DMOC.MerchantOperatingCity) -> m DriverEntityRes
buildDriverEntityRes (person, driverInfo, driverStats, merchantOpCityId) = do
  transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  vehicleMB <- QVehicle.findById person.id
  DriverSpecificSubscriptionData {mbDriverPlan = driverPlan, ..} <- getDriverSpecificSubscriptionDataWithSubsConfig (person.id, transporterConfig.merchantId, merchantOpCityId) transporterConfig driverInfo vehicleMB
  now <- getCurrentTime
  decMobNum <- mapM decrypt person.mobileNumber
  decaltMobNum <- mapM decrypt person.alternateMobileNumber
  let maskedDeviceToken = maskText . (.getFCMRecipientToken) <$> person.deviceToken
  mediaUrl <- forM person.faceImageId $ \mediaId -> do
    mediaEntry <- runInReplica $ MFQuery.findById mediaId >>= fromMaybeM (FileDoNotExist person.id.getId)
    return mediaEntry.url
  aadhaarCardPhotoResp <- try @_ @SomeException (fetchAndCacheAadhaarImage person driverInfo)
  let aadhaarCardPhoto = join (eitherToMaybe aadhaarCardPhotoResp)
  let rating =
        if transporterConfig.ratingAsDecimal
          then SP.roundToOneDecimal <$> driverStats.rating
          else driverStats.rating <&> (\(Centesimal x) -> Centesimal (fromInteger (round x)))
  fareProductConfig <- CQFP.findAllFareProductByMerchantOpCityId person.merchantOperatingCityId
  let supportedServiceTiers = nub $ map (.vehicleServiceTier) fareProductConfig
  (checkIfACWorking, mbDefaultServiceTier) <-
    case vehicleMB of
      Nothing -> return (False, Nothing)
      Just vehicle -> do
        cityServiceTiers <- CQVST.findAllByMerchantOpCityId person.merchantOperatingCityId
        let mbDefaultServiceTierItem = find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
        let checIfACWorking' =
              case mbDefaultServiceTierItem >>= (.airConditionedThreshold) of
                Nothing -> False
                Just acThreshold -> do
                  (fromMaybe 0 driverInfo.airConditionScore) <= acThreshold
                    && maybe True (\lastCheckedAt -> fromInteger (diffDays (utctDay now) (utctDay lastCheckedAt)) >= transporterConfig.acStatusCheckGap) driverInfo.lastACStatusCheckedAt
        return (checIfACWorking', (.serviceTierType) <$> mbDefaultServiceTierItem)
  let isVehicleSupported = maybe False (`elem` supportedServiceTiers) mbDefaultServiceTier
  onRideFlag <-
    if driverInfo.onRide
      then
        Ride.notOnRide person.id >>= \veryMuchNotOnRide ->
          if veryMuchNotOnRide
            then do
              fork "Update Wrongly Set OnRide" $ updateOnRideStatusWithAdvancedRideCheck person.id Nothing
              return False
            else return driverInfo.onRide
      else return driverInfo.onRide
  let driverTags = Yudhishthira.convertTags $ fromMaybe [] person.driverTag
  let mbDriverSafetyTag = Yudhishthira.accessTagKey (LYT.TagName "SafetyCohort") driverTags
      mbDriverSafetyScore = Yudhishthira.accessTagKey (LYT.TagName "SafetyScore") driverTags
      mbDriverOverchargingTag = Yudhishthira.accessTagKey (LYT.TagName "DriverChargingBehaviour") driverTags
      mbTotalRidesConsideredForFareIssues = Yudhishthira.accessTagKey (LYT.TagName "TotalRidesConsideredForFareIssues") driverTags
      mbRidesWithFareIssues = Yudhishthira.accessTagKey (LYT.TagName "RidesWithFareIssue") driverTags
  return $
    DriverEntityRes
      { id = person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        mobileNumber = decMobNum,
        email = person.email,
        rating,
        linkedVehicle = makeVehicleAPIEntity mbDefaultServiceTier <$> vehicleMB,
        active = driverInfo.active,
        onRide = onRideFlag,
        enabled = driverInfo.enabled,
        blocked = driverInfo.blocked,
        blockExpiryTime = driverInfo.blockExpiryTime,
        blockedReasonFlag = driverInfo.blockReasonFlag,
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
        canSwitchToIntraCity = driverInfo.canSwitchToIntraCity,
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
        isVehicleSupported = isVehicleSupported,
        payoutVpa = driverInfo.payoutVpa,
        payoutVpaStatus = driverInfo.payoutVpaStatus,
        payoutVpaBankAccount = driverInfo.payoutVpaBankAccount,
        subscriptionEnabledForVehicleCategory = isEnabledForCategory,
        isSpecialLocWarrior = driverInfo.isSpecialLocWarrior,
        safetyTag = mbDriverSafetyTag,
        safetyScore = mbDriverSafetyScore,
        overchargingTag = mbDriverOverchargingTag,
        ridesWithFareIssues = mbRidesWithFareIssues,
        totalRidesConsideredForFareIssues = mbTotalRidesConsideredForFareIssues,
        softBlockStiers = driverInfo.softBlockStiers,
        softBlockExpiryTime = driverInfo.softBlockExpiryTime,
        softBlockReasonFlag = driverInfo.softBlockReasonFlag,
        onboardingVehicleCategory = driverInfo.onboardingVehicleCategory,
        ..
      }

deleteDriver :: (CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r, MonadReader r m) => SP.Person -> Id SP.Person -> m APISuccess
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
  ( CacheFlow m r,
    EsqDBFlow m r,
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
          canSwitchToIntraCity = fromMaybe driverInfo.canSwitchToIntraCity req.canSwitchToIntraCity
          availableUpiApps = req.availableUpiApps <|> driverInfo.availableUpiApps
          selectedServiceTiers =
            case vehicle.variant of
              DV.AUTO_RICKSHAW -> [DVST.AUTO_RICKSHAW]
              DV.TAXI -> [DVST.TAXI]
              DV.HATCHBACK -> [DVST.HATCHBACK, DVST.ECO] <> [DVST.TAXI | canDowngradeToTaxi]
              DV.SEDAN -> [DVST.SEDAN, DVST.COMFY] <> [DVST.HATCHBACK | canDowngradeToHatchback] <> [DVST.TAXI | canDowngradeToTaxi] <> [DVST.ECO | canDowngradeToHatchback]
              DV.SUV -> [DVST.SUV] <> [DVST.SEDAN | canDowngradeToSedan] <> [DVST.COMFY | canDowngradeToSedan] <> [DVST.HATCHBACK | canDowngradeToHatchback] <> [DVST.TAXI | canDowngradeToTaxi] <> [DVST.ECO | canDowngradeToHatchback]
              DV.TAXI_PLUS -> [DVST.TAXI_PLUS]
              DV.PREMIUM_SEDAN -> [DVST.PREMIUM_SEDAN]
              DV.BLACK -> [DVST.BLACK]
              DV.BLACK_XL -> [DVST.BLACK_XL]
              DV.BIKE -> [DVST.BIKE]
              DV.DELIVERY_BIKE -> [DVST.DELIVERY_BIKE]
              DV.AMBULANCE_TAXI -> [DVST.AMBULANCE_TAXI] -- deprecated, only for compilation
              DV.AMBULANCE_TAXI_OXY -> [DVST.AMBULANCE_TAXI_OXY]
              DV.AMBULANCE_AC -> [DVST.AMBULANCE_AC]
              DV.AMBULANCE_AC_OXY -> [DVST.AMBULANCE_AC_OXY]
              DV.AMBULANCE_VENTILATOR -> [DVST.AMBULANCE_VENTILATOR]
              DV.SUV_PLUS -> [DVST.SUV_PLUS]
              DV.HERITAGE_CAB -> [DVST.HERITAGE_CAB]
              DV.EV_AUTO_RICKSHAW -> [DVST.EV_AUTO_RICKSHAW]
              DV.DELIVERY_LIGHT_GOODS_VEHICLE -> [DVST.DELIVERY_LIGHT_GOODS_VEHICLE]
              DV.DELIVERY_TRUCK_MINI -> [DVST.DELIVERY_TRUCK_MINI]
              DV.DELIVERY_TRUCK_SMALL -> [DVST.DELIVERY_TRUCK_SMALL]
              DV.DELIVERY_TRUCK_MEDIUM -> [DVST.DELIVERY_TRUCK_MEDIUM]
              DV.DELIVERY_TRUCK_LARGE -> [DVST.DELIVERY_TRUCK_LARGE]
              DV.DELIVERY_TRUCK_ULTRA_LARGE -> [DVST.DELIVERY_TRUCK_ULTRA_LARGE]
              DV.BUS_NON_AC -> [DVST.BUS_NON_AC]
              DV.BUS_AC -> [DVST.BUS_AC]

      QDriverInformation.updateDriverInformation canDowngradeToSedan canDowngradeToHatchback canDowngradeToTaxi canSwitchToRental canSwitchToInterCity canSwitchToIntraCity availableUpiApps person.id
      when (isJust req.canDowngradeToSedan || isJust req.canDowngradeToHatchback || isJust req.canDowngradeToTaxi) $
        QVehicle.updateSelectedServiceTiers selectedServiceTiers person.id

  updatedDriverInfo <- QDriverInformation.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  when (isJust req.vehicleName) $ QVehicle.updateVehicleName req.vehicleName personId
  QPerson.updatePersonRec personId updPerson
  driverStats <- runInReplica $ QDriverStats.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  driverEntity <- buildDriverEntityRes (updPerson, updatedDriverInfo, driverStats, merchantOpCityId)
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
        ( (vehicle.variant == DV.AUTO_RICKSHAW || vehicle.variant == DV.TAXI || vehicle.variant == DV.HATCHBACK)
            && (req.canDowngradeToSedan == Just True || req.canDowngradeToHatchback == Just True)
        )
        $ throwError $ InvalidRequest $ "Can't downgrade from " <> (show vehicle.variant)
      when (vehicle.variant == DV.SUV && req.canDowngradeToTaxi == Just True) $
        throwError $ InvalidRequest $ "Can't downgrade to NON-AC TAXI from " <> (show vehicle.variant)
      when
        ( (vehicle.variant == DV.AUTO_RICKSHAW || vehicle.variant == DV.TAXI)
            && (req.canDowngradeToSedan == Just True || req.canDowngradeToHatchback == Just True || req.canDowngradeToTaxi == Just True)
        )
        $ throwError $ InvalidRequest $ "Can't downgrade from " <> (show vehicle.variant)
      when (vehicle.variant == DV.SEDAN && (req.canDowngradeToSedan == Just True)) $
        throwError $ InvalidRequest "Driver with sedan can't downgrade to sedan"
      when (vehicle.variant == DV.TAXI_PLUS && (req.canDowngradeToSedan == Just True || req.canDowngradeToHatchback == Just True)) $
        throwError $ InvalidRequest "Driver with TAXI_PLUS can't downgrade to either sedan or hatchback"

updateMetaData ::
  ( CacheFlow m r,
    EsqDBFlow m r,
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

makeDriverInformationRes :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> DriverEntityRes -> DM.Merchant -> Maybe (Id DR.DriverReferral) -> DriverStats -> DDGR.CachedGoHomeRequest -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe Text -> m DriverInformationRes
makeDriverInformationRes merchantOpCityId DriverEntityRes {..} merchant referralCode driverStats dghInfo currentDues manualDues md5DigestHash = do
  merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  mbVehicle <- QVehicle.findById id
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
  mbPayoutConfig <- CPC.findByPrimaryKey merchantOpCityId vehicleCategory Nothing
  cancellationRateData <- SCR.getCancellationRateData merchantOpCityId id
  bankDetails <-
    if merchant.onlinePayment
      then do
        mbDriverBankAccount <- QDBA.findByPrimaryKey id
        return $ mbDriverBankAccount <&> (\DOBA.DriverBankAccount {..} -> DOVT.BankAccountResp {..})
      else return Nothing
  CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast id))) >>= \cfg ->
    return $
      DriverInformationRes
        { organization = DM.makeMerchantAPIEntity merchant,
          referralCode = referralCode <&> (.getId),
          numberOfRides = driverStats.totalRides,
          driverGoHomeInfo = dghInfo,
          isGoHomeEnabled = cfg.enableGoHome,
          operatingCity = merchantOperatingCity.city,
          frontendConfigHash = md5DigestHash,
          currentDuesWithCurrency = flip PriceAPIEntity merchantOperatingCity.currency <$> currentDues,
          manualDuesWithCurrency = flip PriceAPIEntity merchantOperatingCity.currency <$> manualDues,
          payoutVpa = payoutVpa,
          isPayoutEnabled = mbPayoutConfig <&> (.isPayoutEnabled),
          payoutVpaStatus = payoutVpaStatus,
          payoutRewardAmount = mbPayoutConfig <&> (.referralRewardAmountPerRide),
          payoutVpaBankAccount = payoutVpaBankAccount,
          cancellationRateInWindow = (.cancellationRate) <$> cancellationRateData,
          cancelledRidesCountInWindow = (.cancelledCount) <$> cancellationRateData,
          assignedRidesCountInWindow = (.assignedCount) <$> cancellationRateData,
          windowSize = (.windowSize) <$> cancellationRateData,
          favCount = Just driverStats.favRiderCount,
          ..
        }

getNearbySearchRequests ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r
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
      bapMetadata <- CQSM.findBySubscriberIdAndDomain (Id searchRequest.bapId) Domain.MOBILITY
      isValueAddNP <- CQVAN.isValueAddNP searchRequest.bapId
      farePolicy <- getFarePolicyByEstOrQuoteId (Just $ Maps.getCoordinates searchRequest.fromLocation) searchRequest.fromLocGeohash searchRequest.toLocGeohash searchRequest.estimatedDistance searchRequest.estimatedDuration searchRequest.merchantOperatingCityId searchTry.tripCategory nearbyReq.vehicleServiceTier searchRequest.area (fromMaybe searchTry.estimateId nearbyReq.estimateId) Nothing Nothing searchRequest.dynamicPricingLogicVersion (Just (TransactionId (Id searchRequest.transactionId)))
      popupDelaySeconds <- DP.getPopupDelay merchantOpCityId (cast driverId) cancellationRatio cancellationScoreRelatedConfig transporterConfig.defaultPopupDelay
      let useSilentFCMForForwardBatch = transporterConfig.useSilentFCMForForwardBatch
      let driverPickUpCharges = USRD.extractDriverPickupCharges farePolicy.farePolicyDetails
          parkingCharges = farePolicy.parkingCharge
      return $ USRD.makeSearchRequestForDriverAPIEntity nearbyReq searchRequest searchTry bapMetadata popupDelaySeconds Nothing (Seconds 0) nearbyReq.vehicleServiceTier False isValueAddNP useSilentFCMForForwardBatch driverPickUpCharges parkingCharges -- Seconds 0 as we don't know where he/she lies within the driver pool, anyways this API is not used in prod now.
    mkCancellationScoreRelatedConfig :: TransporterConfig -> CancellationScoreRelatedConfig
    mkCancellationScoreRelatedConfig tc = CancellationScoreRelatedConfig tc.popupDelayToAddAsPenalty tc.thresholdCancellationScore tc.minRidesForCancellationScore

isAllowedExtraFee :: DriverExtraFeeBounds -> HighPrecMoney -> Bool
isAllowedExtraFee extraFee val = extraFee.minFee <= val && val <= extraFee.maxFee

offerQuoteLockKey :: Id Person -> Text
offerQuoteLockKey driverId = "Driver:OfferQuote:DriverId-" <> driverId.getId

-- DEPRECATED
offerQuote :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Text -> DriverOfferReq -> Flow APISuccess
offerQuote (driverId, merchantId, merchantOpCityId) clientId DriverOfferReq {..} = do
  let response = Accept
  respondQuote (driverId, merchantId, merchantOpCityId) clientId Nothing Nothing Nothing Nothing DriverRespondReq {searchRequestId = Nothing, searchTryId = Just searchRequestId, notificationSource = Nothing, renderedAt = Nothing, respondedAt = Nothing, ..}

respondQuote :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Text -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> DriverRespondReq -> Flow APISuccess
respondQuote (driverId, merchantId, merchantOpCityId) clientId mbBundleVersion mbClientVersion mbConfigVersion mbDevice req = do
  searchTryId <- req.searchRequestId <|> req.searchTryId & fromMaybeM (InvalidRequest "searchTryId field is not present.")
  searchTry <- QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
  mSReqFD <- QSRD.findByDriverAndSearchTryId driverId searchTry.id
  sReqFD <-
    case mSReqFD of
      Just srfd -> return srfd
      Nothing -> do
        logWarning $ "Search request not found for the driver with driverId " <> driverId.getId <> " and searchTryId " <> searchTryId.getId
        throwError RideRequestAlreadyAccepted
  case req.response of
    Accept -> do
      quoteRespondCoolDown <- asks (.quoteRespondCoolDown)
      lockRespondQuote <- Redis.tryLockRedis (offerQuoteLockKeyWithCoolDown driverId) quoteRespondCoolDown
      lockEditDestination <- Redis.tryLockRedis (editDestinationLockKey driverId) 10
      callWithErrorHandling $
        if lockRespondQuote && lockEditDestination
          then do
            let reqOfferedValue = (req.offeredFareWithCurrency <&> (.amount)) <|> (toHighPrecMoney <$> req.offeredFare)
            SMerchant.checkCurrencies searchTry.currency [req.offeredFareWithCurrency]
            now <- getCurrentTime
            when (searchTry.validTill < now) $ throwError SearchRequestExpired
            when (sReqFD.isForwardRequest) $ do
              mbGeohash <- Redis.get (editDestinationUpdatedLocGeohashKey driverId)
              when (maybe False (sReqFD.previousDropGeoHash /=) mbGeohash) $ throwError CustomerDestinationUpdated
            let expiryTimeWithBuffer = addUTCTime 10 sReqFD.searchRequestValidTill ------ added 10 secs buffer so that if driver is accepting at last second then because of api latency it sholuldn't fail.
            when (expiryTimeWithBuffer < now) $ throwError (InvalidRequest "Quote can't be responded. SearchReqForDriver is expired")
            searchReq <- QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
            merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantDoesNotExist searchReq.providerId.getId)
            driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
            driverInfo <- QDriverInformation.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
            transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
            throwErrorOnRide transporterConfig.includeDriverCurrentlyOnRide driverInfo sReqFD.isForwardRequest
            when (sReqFD.response == Just Reject) $ do
              throwError QuoteAlreadyRejected
            whenM thereAreActiveQuotes (throwError FoundActiveQuotes)
            driverFCMPulledList <- case DTC.tripCategoryToPricingPolicy searchTry.tripCategory of
              DTC.EstimateBased _ -> acceptDynamicOfferDriverRequest merchant searchTry searchReq driver sReqFD mbBundleVersion mbClientVersion mbConfigVersion mbDevice reqOfferedValue
              DTC.QuoteBased _ -> acceptStaticOfferDriverRequest (Just searchTry) driver (fromMaybe searchTry.estimateId sReqFD.estimateId) reqOfferedValue merchant clientId
            QSRD.updateDriverResponse (Just Accept) Inactive req.notificationSource req.renderedAt req.respondedAt sReqFD.id
            DS.driverScoreEventHandler merchantOpCityId $ buildDriverRespondEventPayload searchTry.id driverFCMPulledList
            unless (sReqFD.isForwardRequest) $ Redis.unlockRedis (editDestinationLockKey driverId)
          else do
            if not lockEditDestination
              then throwError $ DriverTransactionTryAgain Nothing
              else do
                void $ Redis.unlockRedis (editDestinationLockKey driverId)
    Reject -> do
      QSRD.updateDriverResponse (Just Reject) Inactive req.notificationSource req.renderedAt req.respondedAt sReqFD.id
      DP.removeSearchReqIdFromMap merchantId driverId searchTry.id
      unlockRedisQuoteKeys
    Pulled -> do
      QSRD.updateDriverResponse (Just Pulled) Inactive req.notificationSource req.renderedAt req.respondedAt sReqFD.id
      throwError UnexpectedResponseValue
  pure Success
  where
    buildDriverRespondEventPayload searchTryId restActiveDriverSearchReqs =
      DST.OnDriverAcceptingSearchRequest
        { restDriverIds = map (.driverId) restActiveDriverSearchReqs,
          response = req.response,
          ..
        }
    unlockRedisQuoteKeys = do
      Redis.unlockRedis (offerQuoteLockKeyWithCoolDown driverId)
      Redis.unlockRedis (editDestinationLockKey driverId)
    callWithErrorHandling func = do
      exep <- try @_ @SomeException func
      case exep of
        Left e -> do
          unlockRedisQuoteKeys
          someExceptionToAPIErrorThrow e
        Right a -> pure a
    someExceptionToAPIErrorThrow exc
      | Just (HTTPException err) <- fromException exc = throwError err
      | Just (BaseException err) <- fromException exc =
        throwError . InternalError . fromMaybe (show err) $ toMessage err
      | otherwise = throwError . InternalError $ show exc

    buildDriverQuote ::
      (MonadFlow m, MonadReader r m, HasField "driverQuoteExpirationSeconds" r NominalDiffTime, HasField "version" r DeploymentVersion) =>
      SP.Person ->
      DStats.DriverStats ->
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
    buildDriverQuote driver driverStats searchReq sd estimateId tripCategory fareParams mbBundleVersion' mbClientVersion' mbConfigVersion' mbDevice' = do
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
            driverRating = SP.roundToOneDecimal <$> driverStats.rating,
            status = DDrQuote.Active,
            vehicleVariant = sd.vehicleVariant,
            vehicleServiceTier = sd.vehicleServiceTier,
            distance = searchReq.estimatedDistance,
            distanceToPickup = sd.actualDistanceToPickup,
            durationToPickup = sd.durationToPickup,
            currency = sd.currency,
            distanceUnit = sd.distanceUnit,
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
            backendAppVersion = Just deploymentVersion.getDeploymentVersion,
            merchantOperatingCityId = Just searchReq.merchantOperatingCityId,
            vehicleServiceTierName = sd.vehicleServiceTierName
          }
    thereAreActiveQuotes = do
      driverUnlockDelay <- asks (.driverUnlockDelay)
      activeQuotes <- QDrQt.findActiveQuotesByDriverId driverId driverUnlockDelay
      logDebug $ "active quotes for driverId = " <> driverId.getId <> show activeQuotes
      pure $ not $ null activeQuotes
    getQuoteLimit dist vehicleServiceTier tripCategory searchReq area searchRepeatType searchRepeatCounter = do
      driverPoolCfg <- SCDPC.getDriverPoolConfig merchantOpCityId vehicleServiceTier tripCategory area dist searchRepeatType searchRepeatCounter (Just (TransactionId (Id searchReq.transactionId))) searchReq
      pure driverPoolCfg.driverQuoteLimit

    acceptDynamicOfferDriverRequest :: DM.Merchant -> DST.SearchTry -> DSR.SearchRequest -> SP.Person -> SearchRequestForDriver -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe HighPrecMoney -> Flow [SearchRequestForDriver]
    acceptDynamicOfferDriverRequest merchant searchTry searchReq driver sReqFD mbBundleVersion' mbClientVersion' mbConfigVersion' mbDevice' reqOfferedValue = do
      let estimateId = fromMaybe searchTry.estimateId sReqFD.estimateId -- backward compatibility
      logDebug $ "offered fare: " <> show reqOfferedValue
      quoteLimit <- getQuoteLimit searchReq.estimatedDistance sReqFD.vehicleServiceTier searchTry.tripCategory searchReq (fromMaybe SL.Default searchReq.area) searchTry.searchRepeatType searchTry.searchRepeatCounter
      quoteCount <- runInReplica $ QDrQt.countAllBySTId searchTry.id
      driverStats <- runInReplica $ QDriverStats.findById driver.id >>= fromMaybeM DriverInfoNotFound
      when (quoteCount >= quoteLimit) (throwError QuoteAlreadyRejected)
      farePolicy <- getFarePolicyByEstOrQuoteId (Just $ Maps.getCoordinates searchReq.fromLocation) searchReq.fromLocGeohash searchReq.toLocGeohash searchReq.estimatedDistance searchReq.estimatedDuration merchantOpCityId searchTry.tripCategory sReqFD.vehicleServiceTier searchReq.area estimateId Nothing Nothing searchReq.dynamicPricingLogicVersion (Just (TransactionId (Id searchReq.transactionId)))
      let driverExtraFeeBounds = DFarePolicy.findDriverExtraFeeBoundsByDistance (fromMaybe 0 searchReq.estimatedDistance) <$> farePolicy.driverExtraFeeBounds
      whenJust reqOfferedValue $ \off ->
        whenJust driverExtraFeeBounds $ \driverExtraFeeBounds' ->
          unless (isAllowedExtraFee driverExtraFeeBounds' off) $
            throwError $ NotAllowedExtraFee $ show off
      when (searchReq.autoAssignEnabled == Just True) do
        unlessM (CS.lockSearchTry searchTry.id) do
          logError ("RideRequestAlreadyAcceptedOrCancelled " <> "in respond quote for searchTryId:" <> getId searchTry.id <> " estimateId:" <> estimateId <> " driverId:" <> getId driver.id <> " and srfdId:" <> getId sReqFD.id)
          throwError (RideRequestAlreadyAcceptedOrCancelled sReqFD.id.getId)
      fareParams <- do
        calculateFareParameters
          CalculateFareParametersParams
            { farePolicy = farePolicy,
              actualDistance = searchReq.estimatedDistance,
              rideTime = sReqFD.startTime,
              returnTime = searchReq.returnTime,
              roundTrip = fromMaybe False searchReq.roundTrip,
              vehicleAge = sReqFD.vehicleAge,
              waitingTime = Nothing,
              stopWaitingTimes = [],
              noOfStops = length searchReq.stops,
              actualRideDuration = Nothing,
              avgSpeedOfVehicle = Nothing,
              driverSelectedFare = reqOfferedValue,
              customerExtraFee = searchTry.customerExtraFee,
              nightShiftCharge = Nothing,
              customerCancellationDues = searchReq.customerCancellationDues,
              tollCharges = searchReq.tollCharges,
              estimatedRideDuration = searchReq.estimatedDuration,
              nightShiftOverlapChecking = DTC.isFixedNightCharge searchTry.tripCategory,
              estimatedCongestionCharge = Nothing,
              estimatedDistance = searchReq.estimatedDistance,
              timeDiffFromUtc = Nothing,
              currency = searchReq.currency,
              distanceUnit = searchReq.distanceUnit,
              merchantOperatingCityId = Just merchantOpCityId,
              ..
            }
      driverQuote <- buildDriverQuote driver driverStats searchReq sReqFD estimateId searchTry.tripCategory fareParams mbBundleVersion' mbClientVersion' mbConfigVersion' mbDevice'
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

acceptStaticOfferDriverRequest :: Maybe DST.SearchTry -> SP.Person -> Text -> Maybe HighPrecMoney -> DM.Merchant -> Maybe Text -> Flow [SearchRequestForDriver]
acceptStaticOfferDriverRequest mbSearchTry driver quoteId reqOfferedValue merchant clientId = do
  whenJust reqOfferedValue $ \_ -> throwError (InvalidRequest "Driver can't offer rental fare")
  quote <- QQuote.findById (Id quoteId) >>= fromMaybeM (QuoteNotFound quoteId)
  booking <- QBooking.findByQuoteId quote.id.getId >>= fromMaybeM (BookingDoesNotExist quote.id.getId)
  when booking.isScheduled $ removeBookingFromRedis booking
  transporterConfig <- SCTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just (DriverId (cast driver.id))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  isBookingAssignmentInprogress' <- CS.isBookingAssignmentInprogress booking.id
  when isBookingAssignmentInprogress' $ throwError RideRequestAlreadyAccepted
  isBookingCancelled' <- CS.isBookingCancelled booking.id
  when isBookingCancelled' $ throwError (InternalError "BOOKING_CANCELLED")
  CS.markBookingAssignmentInprogress booking.id -- this is to handle booking assignment and user cancellation at same time
  unless (booking.status == DRB.NEW) $ throwError RideRequestAlreadyAccepted
  whenJust mbSearchTry $ \searchTry -> QST.updateStatus DST.COMPLETED searchTry.id
  (ride, _, vehicle) <- initializeRide merchant driver booking Nothing Nothing clientId Nothing
  driverFCMPulledList <-
    case mbSearchTry of
      Just searchTry -> deactivateExistingQuotes booking.merchantOperatingCityId merchant.id driver.id searchTry.id $ mkPrice (Just quote.currency) quote.estimatedFare
      Nothing -> pure []
  uBooking <- QBooking.findById booking.id >>= fromMaybeM (BookingNotFound booking.id.getId)
  handle (errHandler uBooking) $ sendRideAssignedUpdateToBAP uBooking ride driver vehicle False
  when uBooking.isScheduled $ do
    now <- getCurrentTime
    let scheduledPickup = uBooking.fromLocation
        scheduledTime = uBooking.startTime
        pickupPos = LatLong {lat = scheduledPickup.lat, lon = scheduledPickup.lon}
    void $ QDriverInformation.updateLatestScheduledBookingAndPickup (Just scheduledTime) (Just pickupPos) driver.id
    let jobScheduledTime = max 2 ((diffUTCTime uBooking.startTime now) - transporterConfig.scheduleRideBufferTime)
    createJobIn @_ @'ScheduledRideAssignedOnUpdate (Just merchant.id) (Just booking.merchantOperatingCityId) jobScheduledTime $
      ScheduledRideAssignedOnUpdateJobData
        { driverId = driver.id,
          bookingId = uBooking.id,
          rideId = ride.id
        }
  CS.markBookingAssignmentCompleted uBooking.id
  return driverFCMPulledList
  where
    errHandler uBooking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = cancelBooking uBooking (Just driver) merchant >> throwM exc
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = cancelBooking uBooking (Just driver) merchant >> throwM exc
      | otherwise = throwM exc

getStats ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Day ->
  m DriverStatsRes
getStats (driverId, _, merchantOpCityId) date = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  driverDailyStats <- runInReplica $ SQDS.findByDriverIdAndDate driverId date
  coinBalance_ <- Coins.getCoinsByDriverId driverId transporterConfig.timeDiffFromUtc
  validRideCountOfDriver <- fromMaybe 0 <$> Coins.getValidRideCountByDriverIdKey driverId
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId

  let totalEarningsOfDay = maybe 0.0 (.totalEarnings) driverDailyStats
      tipsEarningOfDay = maybe 0.0 (.tipAmount) driverDailyStats
      totalDistanceTravelledInKilometers = maybe 0 (.totalDistance) driverDailyStats `div` 1000
      totalEarningOfDayExcludingTollCharges = totalEarningsOfDay - maybe 0.0 (.tollCharges) driverDailyStats
      bonusEarning = maybe 0.0 (.bonusEarnings) driverDailyStats
      totalEarningsOfDayPerKm =
        if totalDistanceTravelledInKilometers.getMeters == 0
          then HighPrecMoney 0.0
          else toHighPrecMoney $ roundToIntegral totalEarningOfDayExcludingTollCharges `div` totalDistanceTravelledInKilometers.getMeters
  return $
    DriverStatsRes
      { coinBalance = coinBalance_,
        tipsEarning = PriceAPIEntity tipsEarningOfDay currency,
        totalRidesOfDay = maybe 0 (.numRides) driverDailyStats,
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

driverProfileImagesUpload :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Issue.IssueMediaUploadReq -> Flow Issue.IssueMediaUploadRes
driverProfileImagesUpload (driverId, merchantId, merchantOpCityId) Issue.IssueMediaUploadReq {..} = do
  Issue.issueMediaUpload' (cast driverId, cast merchantId, cast merchantOpCityId) driverIssueHandle Issue.IssueMediaUploadReq {..} "driver-profile-images" ("driverId-" <> getId driverId)

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
  createMediaEntry driverId Common.AddLinkAsMedia {url = fileUrl, fileType} filePath
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

createMediaEntry :: Id SP.Person -> Common.AddLinkAsMedia -> Text -> Flow APISuccess
createMediaEntry driverId Common.AddLinkAsMedia {..} filePath = do
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
            s3FilePath = Just filePath,
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
  ( EsqDBFlow m r,
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
  mbPerson <- QPerson.findByMobileNumberAndMerchantAndRole phoneNumber.mobileCountryCode mobileNumberHash person.merchantId SP.DRIVER
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
    withLogTag ("personId_" <> getId person.id) $ do
      (mbSender, message) <-
        MessageBuilder.buildSendAlternateNumberOTPMessage merchantOpCityId $
          MessageBuilder.BuildSendOTPMessageReq
            { otp = otpCode,
              hash = otpHash
            }
      let sender = fromMaybe smsCfg.sender mbSender
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
            { SP.alternateMobileNumber = Just encNewNum
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
    EsqDBFlow m r,
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
  withLogTag ("personId_" <> getId personId) $ do
    (mbSender, message) <-
      MessageBuilder.buildSendAlternateNumberOTPMessage merchantOpCityId $
        MessageBuilder.BuildSendOTPMessageReq
          { otp = otpCode,
            hash = otpHash
          }
    let sender = fromMaybe smsCfg.sender mbSender
    Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message altphoneNumber sender)
      >>= Sms.checkSmsResult
  updAttempts <- Redis.decrby (makeAlternateNumberAttemptsKey personId) 1
  let updAttempt = fromIntegral updAttempts
  return $ ResendAuth {auth = otpCode, attemptsLeft = updAttempt}

remove ::
  ( EsqDBFlow m r,
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
          { SP.alternateMobileNumber = Nothing
          }
  void $ QPerson.updateAlternateMobileNumberAndCode driver
  return Success

-- history should be on basis of invoice instead of driverFee id
getDriverPayments ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) =>
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
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r, EncFlow m r, HasField "smsCfg" r SmsConfig, MonadFlow m) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ServiceNames ->
  Maybe ClearManualSelectedDues ->
  Maybe SPayment.DeepLinkData ->
  m ClearDuesRes
clearDriverDues (personId, _merchantId, opCityId) serviceName clearSelectedReq mbDeepLinkData = do
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCityId serviceName
      >>= fromMaybeM (NoSubscriptionConfigForService opCityId.getId $ show serviceName)
  now <- getCurrentTime
  (dueDriverFees', mKey) <- do
    case clearSelectedReq of
      Just req -> do
        dfees <- QDF.findAllByStatusAndDriverIdWithServiceName personId [DDF.PAYMENT_OVERDUE] (Just $ req.driverFeeIds) serviceName
        let len = length dfees
        let paymentIdLength = length req.driverFeeIds
        unless (len == paymentIdLength) $
          throwError $ InvalidRequest "Status of some id is not PAYMENT_OVERDUE."
        return (dfees, subscriptionConfig.partialDueClearanceMessageKey)
      Nothing -> do
        dfees <- QDF.findAllByStatusAndDriverIdWithServiceName personId [DDF.PAYMENT_OVERDUE] Nothing serviceName
        return (dfees, Nothing)

  --------- to crub up cases related to double debit ----------
  successfulInvoices <- mapM (\fee -> runInReplica (QINV.findActiveManualInvoiceByFeeId fee.id Domain.MANUAL_INVOICE Domain.SUCCESS)) dueDriverFees'
  let allPaidFeeNotMarkedCleared = nub $ map INV.driverFeeId (concat successfulInvoices)
  forM_ allPaidFeeNotMarkedCleared $ \feeId -> QDF.updateStatus DDF.CLEARED feeId now
  let dueDriverFees = filter (\fee -> not $ fee.id `elem` allPaidFeeNotMarkedCleared) dueDriverFees'
  ----------------------------------------------------------
  invoices <- mapM (\fee -> runInReplica (QINV.findActiveManualInvoiceByFeeId fee.id Domain.MANUAL_INVOICE Domain.ACTIVE_INVOICE)) dueDriverFees
  let paymentService = subscriptionConfig.paymentServiceName
      sortedInvoices = mergeSortAndRemoveDuplicate invoices
      splitEnabled = subscriptionConfig.isVendorSplitEnabled == Just True
  vendorFees' <- if splitEnabled then concat <$> mapM (QVF.findAllByDriverFeeId . DDF.id) dueDriverFees else pure []
  let vendorFees = map SPayment.roundVendorFee vendorFees'
  clearDueResp <- do
    case sortedInvoices of
      [] -> mkClearDuesResp <$> SPayment.createOrder (personId, _merchantId, opCityId) paymentService (dueDriverFees, []) Nothing INV.MANUAL_INVOICE Nothing vendorFees mbDeepLinkData splitEnabled
      (invoice_ : restinvoices) -> do
        mapM_ (QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE . (.id)) restinvoices
        (invoice, currentDuesForExistingInvoice, newDues) <- validateExistingInvoice invoice_ dueDriverFees
        let driverFeeForCurrentInvoice = filter (\dfee -> dfee.id.getId `elem` currentDuesForExistingInvoice) dueDriverFees
        let driverFeeToBeAddedOnExpiry = filter (\dfee -> dfee.id.getId `elem` newDues) dueDriverFees
        mkClearDuesResp <$> SPayment.createOrder (personId, _merchantId, opCityId) paymentService (driverFeeForCurrentInvoice, driverFeeToBeAddedOnExpiry) Nothing INV.MANUAL_INVOICE invoice vendorFees mbDeepLinkData splitEnabled
  let mbPaymentLink = clearDueResp.orderResp.payment_links
      payload = clearDueResp.orderResp.sdk_payload.payload
      mbAmount = readMaybe (T.unpack payload.amount) :: Maybe HighPrecMoney
  whenJust mKey $ \messageKey -> do
    fork "send link through dasboard" $ do
      SPayment.sendLinkTroughChannelProvided mbPaymentLink personId mbAmount (Just subscriptionConfig.paymentLinkChannel) False messageKey
  return clearDueResp
  where
    validateExistingInvoice invoice driverFees = do
      invoices <- runInReplica $ QINV.findAllByInvoiceId invoice.id
      let driverFeeIds = driverFees <&> getId . (.id)
      let currentDueDriverFee = (invoices <&> getId . (.driverFeeId)) `intersect` driverFeeIds
      if isJust clearSelectedReq
        then do
          QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE invoice.id
          return (Nothing, driverFeeIds, [])
        else
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
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) =>
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

mkManualPaymentEntity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => INV.Invoice -> Map (Id DDF.DriverFee) DDF.DriverFee -> TransporterConfig -> m (Maybe ManualInvoiceHistory)
mkManualPaymentEntity manualInvoice mapDriverFeeByDriverFeeId' transporterConfig = do
  allEntriesByInvoiceId <- QINV.findAllByInvoiceId manualInvoice.id
  allDriverFeeForInvoice <- QDF.findAllByDriverFeeIds (allEntriesByInvoiceId <&> (.driverFeeId))
  let amount = sum $ mapToAmount $ filter ((DDF.CLEARED_BY_YATRI_COINS /=) . (.status)) allDriverFeeForInvoice
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
                  amount = sum $ mapToAmount $ filter ((DDF.CLEARED_BY_YATRI_COINS /=) . (.status)) [dfee],
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
    mapToAmount = map (\dueDfee -> SLDriverFee.roundToHalf dueDfee.currency (dueDfee.govtCharges + dueDfee.platformFee.fee + dueDfee.platformFee.cgst + dueDfee.platformFee.sgst)) . filter ((DDF.CLEARED_BY_YATRI_COINS /=) . (.status))

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
    gst :: HighPrecMoney,
    gstWithCurrency :: PriceAPIEntity,
    gstPercentage :: HighPrecMoney,
    vehicleNumber :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data SecurityDepositDfStatusRes = SecurityDepositDfStatusRes
  { securityDepositStatus :: DDF.DriverFeeStatus,
    securityDepositAmountWithCurrency :: Maybe PriceAPIEntity,
    driverFeeId :: Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

getHistoryEntryDetailsEntityV2 ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) =>
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
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
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
            driverFeeAmount =
              if driverFee.status == DDF.CLEARED_BY_YATRI_COINS
                then 0.0
                else SLDriverFee.roundToHalf driverFee.currency (driverFee.govtCharges + driverFee.platformFee.fee + driverFee.platformFee.cgst + driverFee.platformFee.sgst)
            cgst = maybe 0.0 (.cgstPercentage) mbPlan
            sgst = maybe 0.0 (.sgstPercentage) mbPlan
            gst = (driverFeeAmount + fromMaybe 0.0 driverFee.amountPaidByCoin) * (cgst + sgst)
        return
          DriverFeeInfoEntity
            { autoPayStage = driverFee.autopayPaymentStage,
              paymentStatus = invoiceStatus,
              totalEarnings = driverFee.totalEarnings,
              totalEarningsWithCurrency = PriceAPIEntity driverFee.totalEarnings driverFee.currency,
              driverFeeAmount = driverFeeAmount,
              driverFeeAmountWithCurrency = PriceAPIEntity driverFeeAmount driverFee.currency,
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
              gstWithCurrency = PriceAPIEntity gst driverFee.currency,
              gst = gst,
              gstPercentage = cgst + sgst,
              maxRidesEligibleForCharge
            }
    )
    driverFees

getCity :: GetCityReq -> Flow GetCityResp
getCity req = do
  let latLng = LatLong {lat = req.lat, lon = req.lon}
  case req.merchantId of -- only for backward compatibility, Nothing part to be removed later
    Just mId -> do
      merchant <- CQM.findById mId >>= fromMaybeM (MerchantDoesNotExist mId.getId)
      nearestAndSourceCity <- try @_ @SomeException $ getNearestOperatingAndSourceCity merchant latLng
      case nearestAndSourceCity of
        Left _ -> return GetCityResp {city = Nothing, status = APISuccess.Success}
        Right nearestSourceCity -> return GetCityResp {city = Just $ show nearestSourceCity.nearestOperatingCity.city, status = APISuccess.Success}
    Nothing -> do
      geometry <- runInReplica $ QGeometry.findGeometriesContainingGps latLng
      case filter (\geom -> geom.city /= Context.AnyCity) geometry of
        [] ->
          find (\geom -> geom.city == Context.AnyCity) geometry & \case
            Just anyCityGeom -> return GetCityResp {city = Just $ show anyCityGeom.city, status = APISuccess.Success}
            Nothing -> return GetCityResp {city = Nothing, status = APISuccess.Success}
        (g : _) -> return GetCityResp {city = Just $ show g.city, status = APISuccess.Success}

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

getDownloadInvoiceData :: (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Day -> Maybe Day -> m [DriverFeeResp]
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
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m APISuccess
getDummyRideRequest (personId, _, merchantOpCityId) = do
  driver <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  DriverNotify.triggerDummyRideRequest driver merchantOpCityId False

listScheduledBookings ::
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Day ->
  Maybe Day ->
  Maybe DTC.TripCategory ->
  Maybe LatLong ->
  Flow ScheduledBookingRes
listScheduledBookings (personId, _, cityId) mbLimit mbOffset mbFromDay mbToDay mbTripCategory mbDLoc = do
  transporterConfig <- SCTC.findByMerchantOpCityId cityId Nothing >>= fromMaybeM (TransporterConfigNotFound cityId.getId)
  if transporterConfig.disableListScheduledBookingAPI
    then pure $ ScheduledBookingRes []
    else do
      driverInfo <- runInReplica $ QDriverInformation.findById personId >>= fromMaybeM DriverInfoNotFound
      let isDriverOnline = driverInfo.mode `elem` [Just DriverInfo.ONLINE, Just DriverInfo.SILENT]
      mbDLoc' <- do
        case (mbDLoc, isDriverOnline) of
          (Just dLoc, _) -> pure $ Just dLoc
          (Nothing, True) -> getCurrentDriverLocUsingLTS personId
          _ -> pure Nothing
      case (mbFromDay, mbToDay, mbDLoc') of
        (Just from, Just to, Just dLoc) -> do
          when (from > to) $ throwError (InvalidRequest "From date should be less than to date")
          case driverInfo.latestScheduledBooking of
            Just _ -> pure $ ScheduledBookingRes []
            Nothing -> do
              now <- getCurrentTime
              vehicle <- runInReplica $ QVehicle.findById personId >>= fromMaybeM (VehicleDoesNotExist personId.getId)
              -- driverStats <- runInReplica $ QDriverStats.findById vehicle.driverId >>= fromMaybeM DriverInfoNotFound
              cityServiceTiers <- CQVST.findAllByMerchantOpCityId cityId
              let availableServiceTiers = (.serviceTierType) <$> (map fst $ filter (not . snd) (selectVehicleTierForDriverWithUsageRestriction False driverInfo vehicle cityServiceTiers))
                  vehicleVariants = nub $ castServiceTierToVariant <$> availableServiceTiers
                  tripCategory = maybe possibleScheduledTripCategories (: []) mbTripCategory
                  currentDay = utctDay now
                  limit = fromMaybe 10 mbLimit
                  offset = fromMaybe 0 mbOffset
                  safelimit = toInteger transporterConfig.recentScheduledBookingsSafeLimit

              scheduledBookings <-
                if currentDay >= from
                  then getTodayScheduledBookings now cityId vehicleVariants dLoc vehicle transporterConfig availableServiceTiers tripCategory limit offset safelimit
                  else getTommorowScheduledBookings now (UTCTime from 0) cityId vehicleVariants dLoc vehicle transporterConfig availableServiceTiers tripCategory limit offset

              bookings <- mapM (buildBookingAPIEntityFromBooking mbDLoc) (catMaybes scheduledBookings)
              let sortedBookings = sortBookingsByDistance (catMaybes bookings)
              return $ ScheduledBookingRes sortedBookings
        _ -> pure $ ScheduledBookingRes []
  where
    getCurrentDriverLocUsingLTS driverId = do
      result <- try @_ @SomeException $ LTF.driversLocation [driverId]
      return $ case result of
        Left _ -> Nothing
        Right locations -> listToMaybe locations >>= \x -> Just LatLong {lat = x.lat, lon = x.lon}

    possibleScheduledTripCategories :: [DTC.TripCategory]
    possibleScheduledTripCategories = [DTC.Rental DTC.OnDemandStaticOffer, DTC.InterCity DTC.OneWayOnDemandStaticOffer Nothing]

    filterNearbyBookings :: UTCTime -> LatLong -> DV.VehicleVariant -> Maybe AvgSpeedOfVechilePerKm -> [(Text, Double, Double, UTCTime, ServiceTierType)] -> [ServiceTierType] -> [Text]
    filterNearbyBookings currentTime dLoc variant avgSpeeds parsedRes possibleServiceTierTypes = map (\(id, _, _, _, _) -> id) $ filter (\(_, lat, lon, pickupTime, bookingServiceTier) -> checkNearbyBookingsWithServiceTier currentTime pickupTime lat lon dLoc variant avgSpeeds possibleServiceTierTypes bookingServiceTier) parsedRes

    parseMember :: Text -> Maybe (Text, Double, Double, UTCTime, ServiceTierType)
    parseMember member = do
      let parts = T.splitOn "|" member
      case parts of
        [idText, latText, lonText, startTimeText, serviceTierTypeText] -> do
          lat <- readMaybe (T.unpack latText) :: Maybe Double
          lon <- readMaybe (T.unpack lonText) :: Maybe Double
          startTime <- parseTimeM True defaultTimeLocale "%FT%T%z" (T.unpack startTimeText)
          serviceTierType <- readMaybe (T.unpack serviceTierTypeText) :: Maybe ServiceTierType
          return (idText, lat, lon, startTime, serviceTierType)
        _ -> Nothing

    checkNearbyBookingsWithServiceTier :: UTCTime -> UTCTime -> Double -> Double -> LatLong -> DV.VehicleVariant -> Maybe AvgSpeedOfVechilePerKm -> [ServiceTierType] -> ServiceTierType -> Bool
    checkNearbyBookingsWithServiceTier currentTime pickupTime lat lon dLoc variant avgSpeeds possibleServiceTierTypes bookingServiceTierType =
      let bookingLoc = LatLong {..}
          distanceToPickup = highPrecMetersToMeters $ distanceBetweenInMeters bookingLoc dLoc
          distanceInKm = metersToKilometers distanceToPickup
          avgSpeedOfVehicleInKM = maybe 0 (SDP.getVehicleAvgSpeed variant) avgSpeeds
          speedInMinPerKm = if avgSpeedOfVehicleInKM.getKilometers == 0 then 3 else truncate (60 / (toRational avgSpeedOfVehicleInKM.getKilometers))
          estimatedTime = intToNominalDiffTime $ distanceInKm.getKilometers * speedInMinPerKm * 60
          isValidServiceTierType = bookingServiceTierType `elem` possibleServiceTierTypes
       in (isValidServiceTierType && addUTCTime estimatedTime currentTime <= pickupTime)

    sortBookingsByDistance :: [ScheduleBooking] -> [ScheduleBooking]
    sortBookingsByDistance = sortBy (compareDistances `on` (\booking -> booking.bookingDetails.distanceToPickup))

    compareDistances :: Maybe Meters -> Maybe Meters -> Ordering
    compareDistances (Just d1) (Just d2) = compare (getMeters d1) (getMeters d2)
    compareDistances (Just _) Nothing = LT
    compareDistances Nothing (Just _) = GT
    compareDistances Nothing Nothing = EQ

    buildBookingAPIEntityFromBooking mbDriverLocation DRB.Booking {..} = do
      let pickup = LatLong {lat = fromLocation.lat, lon = fromLocation.lon}
          distanceToPickup' = highPrecMetersToMeters . (`distanceBetweenInMeters` pickup) <$> mbDriverLocation
      mbQuote <- QQuote.findById (Id quoteId)
      case mbQuote of
        Nothing -> do
          fork "Error in case of no quote - Potential drainer lag" $ throwError (ShouldNotHappen $ "Quote with quoteId = \"" <> quoteId <> "\" not found.")
          pure Nothing
        Just quote -> do
          let farePolicyBreakups = maybe [] (mkFarePolicyBreakups Prelude.id mkBreakupItem estimatedDistance Nothing estimatedFare quote.fareParams.congestionChargeViaDp) quote.farePolicy
          return $ Just $ ScheduleBooking BookingAPIEntity {distanceToPickup = distanceToPickup', ..} (catMaybes farePolicyBreakups)

    mkBreakupItem :: Text -> Text -> Maybe DOVT.RateCardItem
    mkBreakupItem title valueInText = do
      priceObject <- DOV.stringToPrice INR valueInText
      return $
        DOVT.RateCardItem
          { title,
            price = priceObject.amountInt,
            priceWithCurrency = mkPriceAPIEntity priceObject
          }
    getTodayScheduledBookings :: UTCTime -> Id DMOC.MerchantOperatingCity -> [VehicleVariant] -> LatLong -> Vehicle -> TransporterConfig -> [ServiceTierType] -> [DTC.TripCategory] -> Integer -> Integer -> Integer -> Flow [Maybe DRB.Booking]
    getTodayScheduledBookings now mocCityId vehicleVariants dLoc vehicle transporterConfig possibleServiceTierTypes tripCategories limit offset safelimit = do
      let nextDay = addDays 1 (utctDay now)
          nextDayStartTime = UTCTime nextDay 0
          redisKeys = createRedisKeysForCombinations now mocCityId tripCategories vehicleVariants
          redisKeyForHset = createRedisKeyForHset now mocCityId
          startScore = calculateSortedSetScore $ addUTCTime 1800 now
          endScore = calculateSortedSetScore $ addUTCTime (3600 * 2) now
          startScore2 = calculateSortedSetScore $ addUTCTime ((3600 * 2) + 1) now
          end = calculateSortedSetScore nextDayStartTime

      res <- mapM (\key -> Redis.zRangeByScoreByCount key startScore endScore offset safelimit) redisKeys
      res2 <- mapM (\key -> Redis.zRangeByScoreByCount key startScore2 end offset limit) redisKeys

      returnFilteredBookings now (concat (res ++ res2)) dLoc vehicle.variant transporterConfig.avgSpeedOfVehicle possibleServiceTierTypes redisKeyForHset limit

    getTommorowScheduledBookings :: UTCTime -> UTCTime -> Id DMOC.MerchantOperatingCity -> [VehicleVariant] -> LatLong -> Vehicle -> TransporterConfig -> [ServiceTierType] -> [DTC.TripCategory] -> Integer -> Integer -> Flow [Maybe DRB.Booking]
    getTommorowScheduledBookings now dayStartTime mocCityId vehicleVariants dLoc vehicle transporterConfig possibleServiceTierTypes tripCategories limit offset = do
      let redisKeys = createRedisKeysForCombinations dayStartTime mocCityId tripCategories vehicleVariants
          redisKeyForHset = createRedisKeyForHset dayStartTime mocCityId
          startScore = calculateSortedSetScore dayStartTime
          endScore = calculateSortedSetScore $ addUTCTime (3600 * 24) dayStartTime

      res <- mapM (\key -> Redis.zRangeByScoreByCount key startScore endScore offset limit) redisKeys

      returnFilteredBookings now (concat res) dLoc vehicle.variant transporterConfig.avgSpeedOfVehicle possibleServiceTierTypes redisKeyForHset limit

    returnFilteredBookings :: UTCTime -> [BS.ByteString] -> LatLong -> VehicleVariant -> Maybe AvgSpeedOfVechilePerKm -> [ServiceTierType] -> Text -> Integer -> Flow [Maybe DRB.Booking]
    returnFilteredBookings now res dLoc variant avgSpeedOfVehicle possibleServiceTierTypes redisKeyForHset limit = do
      let parsedRes = mapMaybe (parseMember . decodeUtf8) res
          nearbyBookings = take (fromIntegral limit) $ filterNearbyBookings now dLoc variant avgSpeedOfVehicle parsedRes possibleServiceTierTypes
      if not $ null nearbyBookings
        then Redis.hmGet redisKeyForHset nearbyBookings
        else pure []

acceptScheduledBooking ::
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Text ->
  Id DRB.Booking ->
  Flow APISuccess
acceptScheduledBooking (personId, merchantId, _) clientId bookingId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  booking <- runInReplica $ QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  driver <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  upcomingOrActiveRide <- runInReplica $ QRide.getUpcomingOrActiveByDriverId driver.id
  unless (isNothing upcomingOrActiveRide) $ throwError (RideInvalidStatus "Cannot accept booking during active or already having upcoming ride.")
  mbActiveSearchTry <- QST.findActiveTryByQuoteId booking.quoteId
  void $ acceptStaticOfferDriverRequest mbActiveSearchTry driver booking.quoteId Nothing merchant clientId
  pure Success

clearDriverFeeWithCreate ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r, HasField "smsCfg" r SmsConfig) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ServiceNames ->
  (HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney) ->
  DDF.FeeType ->
  Currency ->
  Maybe SPayment.DeepLinkData ->
  Bool ->
  m ClearDuesRes
clearDriverFeeWithCreate (personId, merchantId, opCityId) serviceName (fee', mbCgst, mbSgst) feeType currency mbDeepLinkData sendPaymentLink = do
  dueDriverFee <- QDFE.findAllByStatusAndDriverIdWithServiceNameFeetype personId [DDF.PAYMENT_PENDING] feeType serviceName
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCityId serviceName
      >>= fromMaybeM (NoSubscriptionConfigForService opCityId.getId $ show serviceName)
  (sgst, cgst) <- case gstPercentagesByFeeTypeAndServiceName feeType subscriptionConfig of
    Just (sgstPercentage, cgstPercentage) -> do
      let sgst' = fromMaybe (calcPercentage sgstPercentage) mbSgst
          cgst' = fromMaybe (calcPercentage cgstPercentage) mbCgst
      return (sgst', cgst')
    Nothing -> return (0.0, 0.0)
  mbVehicle <- runInReplica $ QVehicle.findById personId
  let vehicleCategory = fromMaybe subscriptionConfig.defaultCityVehicleCategory (mbVehicle >>= (.category))
  let fee = fee' - if isJust mbSgst && isJust mbCgst then 0.0 else sgst + cgst
  driverFee <-
    case dueDriverFee of
      [] -> do
        driverFee' <- mkDriverFee fee cgst sgst vehicleCategory
        QDF.create driverFee'
        pure [driverFee']
      dfee -> pure dfee
  invoices <- mapM (\fee_ -> runInReplica (QINV.findActiveManualInvoiceByFeeId fee_.id (feeTypeToInvoicetype feeType) Domain.ACTIVE_INVOICE)) driverFee
  let paymentService = subscriptionConfig.paymentServiceName
      sortedInvoices = mergeSortAndRemoveDuplicate invoices
      splitEnabled = subscriptionConfig.isVendorSplitEnabled == Just True
  vendorFees' <- if splitEnabled then concat <$> mapM (QVF.findAllByDriverFeeId . DDF.id) driverFee else pure []
  let vendorFees = map SPayment.roundVendorFee vendorFees'
  resp <- do
    case sortedInvoices of
      [] -> do mkClearDuesResp <$> SPayment.createOrder (personId, merchantId, opCityId) paymentService (driverFee, []) Nothing (feeTypeToInvoicetype feeType) Nothing vendorFees mbDeepLinkData splitEnabled
      (invoice_ : restinvoices) -> do
        mapM_ (QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE . (.id)) restinvoices
        (invoice, currentDuesForExistingInvoice, newDues) <- validateExistingInvoice invoice_ driverFee
        let driverFeeForCurrentInvoice = filter (\dfee -> dfee.id.getId `elem` currentDuesForExistingInvoice) driverFee
        let driverFeeToBeAddedOnExpiry = filter (\dfee -> dfee.id.getId `elem` newDues) driverFee
        mkClearDuesResp <$> SPayment.createOrder (personId, merchantId, opCityId) paymentService (driverFeeForCurrentInvoice, driverFeeToBeAddedOnExpiry) Nothing (feeTypeToInvoicetype feeType) invoice vendorFees mbDeepLinkData splitEnabled
  let mbPaymentLink = resp.orderResp.payment_links
      payload = resp.orderResp.sdk_payload.payload
      mbAmount = readMaybe (T.unpack payload.amount) :: Maybe HighPrecMoney
      mbMessageKey = messageByFeeType feeType
  whenJust mbMessageKey $ \messageKey -> do
    when sendPaymentLink $ do
      fork "send link through dasboard" $ do
        SPayment.sendLinkTroughChannelProvided mbPaymentLink personId mbAmount (Just subscriptionConfig.paymentLinkChannel) False messageKey
  return resp
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
    mkDriverFee fee cgst sgst vehicleCategory = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DDF.DriverFee
          { id = id,
            merchantId = merchantId,
            payBy = now,
            status = DDF.PAYMENT_PENDING,
            numRides = 0,
            createdAt = now,
            updatedAt = now,
            platformFee = DDF.PlatformFee {fee, cgst, sgst, currency},
            totalEarnings = 0,
            feeType = feeType,
            govtCharges = 0,
            startTime = now,
            endTime = now,
            collectedBy = Nothing,
            driverId = cast personId,
            offerId = Nothing,
            planOfferTitle = Nothing, -- change
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
            planId = Nothing,
            planMode = Nothing,
            notificationRetryCount = 0,
            badDebtDeclarationDate = Nothing,
            vehicleNumber = Nothing,
            badDebtRecoveryDate = Nothing,
            merchantOperatingCityId = opCityId,
            serviceName,
            refundedBy = Nothing,
            refundEntityId = Nothing,
            refundedAt = Nothing,
            refundedAmount = Nothing,
            hasSibling = Nothing,
            siblingFeeId = Nothing,
            splitOfDriverFeeId = Nothing,
            vehicleCategory = vehicleCategory,
            currency
          }
    mkClearDuesResp (orderResp, orderId) = ClearDuesRes {orderId = orderId, orderResp}

    feeTypeToInvoicetype driverFeeType =
      case driverFeeType of
        DDF.ONE_TIME_SECURITY_DEPOSIT -> Domain.ONE_TIME_SECURITY_INVOICE
        DDF.PAYOUT_REGISTRATION -> Domain.PAYOUT_REGISTRATION_INVOICE
        _ -> Domain.MANUAL_INVOICE

    messageByFeeType driverFeeType =
      case (driverFeeType, serviceName) of
        (DDF.ONE_TIME_SECURITY_DEPOSIT, YATRI_RENTAL) -> Just DTM.WHATSAPP_SEND_ONE_TIME_SECURITY_PAYMENT_LINK
        (DDF.RECURRING_INVOICE, YATRI_RENTAL) -> Just DTM.WHATSAPP_SEND_MANUAL_PAYMENT_LINK
        (_, _) -> Nothing

    gstPercentagesByFeeTypeAndServiceName driverFeeType subscriptionConfig =
      case (driverFeeType, serviceName) of
        (DDF.ONE_TIME_SECURITY_DEPOSIT, YATRI_RENTAL) -> (,) <$> subscriptionConfig.sgstPercentageOneTimeSecurityDeposit <*> subscriptionConfig.cgstPercentageOneTimeSecurityDeposit
        (_, _) -> Just (0.0, 0.0)

    calcPercentage percentage = (percentage * fee') / 100.0

verifyVpaStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> m APISuccess
verifyVpaStatus (personId, _, opCityId) = do
  void $ QDriverInformation.updatePayoutVpaStatus (Just DriverInfo.VERIFIED_BY_USER) personId
  driverInfo <- QDriverInformation.findById personId >>= fromMaybeM DriverInfoNotFound
  fork ("processing backlog payout for driver via verify vpaStatus " <> personId.getId) $ Payout.processPreviousPayoutAmount (cast personId) driverInfo.payoutVpa opCityId
  pure Success

getSecurityDepositDfStatus ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ServiceNames ->
  m [SecurityDepositDfStatusRes]
getSecurityDepositDfStatus (personId, _, _) serviceName = do
  driverFees <- runInReplica $ QDFE.findAllByStatusAndDriverIdWithServiceNameFeetype personId [DDF.PAYMENT_PENDING, DDF.CLEARED, DDF.PAYMENT_OVERDUE, DDF.EXEMPTED, DDF.COLLECTED_CASH] DDF.ONE_TIME_SECURITY_DEPOSIT serviceName
  mapM buildSecurityDepositDfStatus $ sortOn (.createdAt) driverFees
  where
    buildSecurityDepositDfStatus dfee = do
      let securityDepositAmount = SLDriverFee.roundToHalf dfee.currency dfee.govtCharges + dfee.platformFee.fee + dfee.platformFee.cgst + dfee.platformFee.sgst
          securityDepositAmountWithCurrency = Just $ PriceAPIEntity securityDepositAmount dfee.currency
      return $
        SecurityDepositDfStatusRes
          { securityDepositStatus = dfee.status,
            driverFeeId = dfee.id.getId,
            createdAt = dfee.createdAt,
            ..
          }

data RefundByPayoutReq = RefundByPayoutReq
  { serviceName :: ServiceNames,
    refundAmountDeduction :: HighPrecMoney,
    payerVpa :: Maybe Text,
    refundAmountSegregation :: Maybe Text,
    driverFeeType :: DDF.FeeType
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

mkPayoutLockKeyByDriverAndService :: Id SP.Person -> ServiceNames -> Text
mkPayoutLockKeyByDriverAndService person serviceName = "POUT:REF:DRIVER:ID:" <> person.getId <> ":SN:" <> show serviceName

refundByPayoutDriverFee :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> RefundByPayoutReq -> Flow APISuccess
refundByPayoutDriverFee (personId, _, opCityId) refundByPayoutReq = do
  let serviceName = refundByPayoutReq.serviceName
  Redis.whenWithLockRedis (mkPayoutLockKeyByDriverAndService personId serviceName) 60 $ do
    let driverFeeType = refundByPayoutReq.driverFeeType
        refundAmountDeduction = refundByPayoutReq.refundAmountDeduction
    (_, mDriverPlan) <- DAPlan.getSubcriptionStatusWithPlan serviceName personId
    driverInfo <- QDriverInformation.findById personId >>= fromMaybeM DriverInfoNotFound
    ------------ todo :-  put check for access post rbac implemtation --------------
    let mbVpa = refundByPayoutReq.payerVpa <|> driverInfo.payoutVpa <|> (mDriverPlan >>= (.payerVpa))
    unless (isJust mbVpa) $ throwError (InternalError $ "payer vpa not present for " <> personId.getId)
    whenJust mbVpa $ \vpa -> do
      pendingDriverFees <- runInReplica $ QDFE.findAllByStatusAndDriverIdWithServiceNameFeetype personId [DDF.PAYMENT_PENDING] DDF.RECURRING_EXECUTION_INVOICE serviceName
      unless (null pendingDriverFees) $ throwError (InternalError "some driver fee currently in auto pay execution")
      driverFees <- runInReplica $ QDFE.findAllByStatusAndDriverIdWithServiceNameFeetype personId [DDF.CLEARED, DDF.REFUND_FAILED, DDF.COLLECTED_CASH] driverFeeType serviceName
      dueDriverFees <- QDF.findAllByStatusAndDriverIdWithServiceName personId [DDF.PAYMENT_OVERDUE] Nothing serviceName
      let totalSecurityDeposit = sum $ map mapToAmount driverFees
          dueDriverFeesAmount = sum $ map mapToAmount dueDriverFees
          refundAmount = totalSecurityDeposit - dueDriverFeesAmount - refundAmountDeduction
      when (refundAmount < 0.0) $ throwError (InternalError "refund amount is less than 0")
      let driverFeeSorted = sortOn (.platformFee.fee) driverFees
      subscriptionConfig <- do
        CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCityId serviceName
          >>= fromMaybeM (NoSubscriptionConfigForService opCityId.getId $ show serviceName)
      uid <- generateGUID
      let ((driverFeeToPayout, driverFeeToSettle), _) = driverFeeWithRefundData driverFeeSorted refundAmount uid
      person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      phoneNo <- mapM decrypt person.mobileNumber
      let createPayoutOrderReq = mkPayoutReq driverFeeToPayout person vpa uid phoneNo
          payoutServiceName = fromMaybe (DEMSC.PayoutService TPayout.Juspay) subscriptionConfig.payoutServiceName
          entityName = DPayment.DRIVER_FEE
          createPayoutOrderCall = Payout.createPayoutOrder person.merchantId opCityId payoutServiceName
      merchantOperatingCity <- CQMOC.findById (cast opCityId) >>= fromMaybeM (MerchantOperatingCityNotFound opCityId.getId)
      logDebug $ "calling create payoutOrder with driverId: " <> personId.getId <> " | amount: " <> show createPayoutOrderReq.amount <> " | orderId: " <> show uid
      when (createPayoutOrderReq.amount < 0.0) $ throwError (InternalError "refund amount is less than 0")
      (_, mbPayoutOrder) <- do
        if createPayoutOrderReq.amount > 0.0
          then DPayment.createPayoutService (cast person.merchantId) (Just $ cast person.merchantOperatingCityId) (cast personId) (Just $ map ((.getId) . (.id)) driverFeeToPayout) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall
          else pure (Nothing, Nothing)
      whenJust mbPayoutOrder $ \payoutOrder -> do
        let refundAmountSegregation = fromMaybe "NA" refundByPayoutReq.refundAmountSegregation
        ET.trackRefundSegregation payoutOrder refundAmountSegregation (show serviceName)
      when (createPayoutOrderReq.amount >= 0.0) $ do
        forM_ driverFeeToPayout $ \refundFee -> do
          let refundData =
                DDF.RefundInfo
                  { refundedBy = refundFee.refundedBy,
                    refundEntityId = refundFee.refundEntityId,
                    refundedAt = refundFee.refundedAt,
                    status = Just refundFee.status,
                    refundedAmount = refundFee.refundedAmount
                  }
          QDF.updateRefundData refundFee.id refundData
        forM_ driverFeeToSettle $ \settleFee -> do
          QDF.updateStatus settleFee.status settleFee.id =<< getCurrentTime
        when (createPayoutOrderReq.amount == 0.0) $ SLDriverFee.adjustDues dueDriverFees
  return Success
  where
    mapToAmount = \dueDfee -> SLDriverFee.roundToHalf dueDfee.currency (dueDfee.govtCharges + dueDfee.platformFee.fee + dueDfee.platformFee.cgst + dueDfee.platformFee.sgst)
    driverFeeWithRefundData driverFeeSorted refundAmount uid =
      foldl'
        ( \acc dfee@DDF.DriverFee {serviceName = planServiceName, ..} -> do
            let amount = mapToAmount dfee
            let driverFeesToAccumulate = fst acc
            if snd acc > 0.0
              then do
                let dfee' =
                      DDF.DriverFee
                        { status = DDF.REFUND_PENDING,
                          refundedBy = Just DDF.PAYOUT,
                          refundEntityId = Just uid,
                          refundedAmount = Just $ min (snd acc) amount,
                          serviceName = planServiceName,
                          ..
                        }
                ((dfee' : fst driverFeesToAccumulate, snd driverFeesToAccumulate), snd acc - fromMaybe 0.0 dfee'.refundedAmount)
              else do
                let dfee' = DDF.DriverFee {status = DDF.SETTLED, serviceName = planServiceName, ..}
                ((fst driverFeesToAccumulate, dfee' : snd driverFeesToAccumulate), snd acc - fromMaybe 0.0 dfee'.refundedAmount)
        )
        (([], []), refundAmount)
        driverFeeSorted
    mkPayoutReq driverFeeToPayout person vpa uid phoneNo =
      Juspay.CreatePayoutOrderReq
        { orderId = uid,
          amount = foldl (\acc dfee -> acc + fromMaybe 0.0 dfee.refundedAmount) 0.0 driverFeeToPayout,
          customerPhone = fromMaybe "6666666666" phoneNo, -- dummy no.
          customerEmail = fromMaybe "dummymail@gmail.com" person.email, -- dummy mail
          customerId = personId.getId,
          orderType = "FULFILL_ONLY",
          remark = "Refund for security deposit",
          customerName = person.firstName,
          customerVpa = vpa
        }

isPlanVehCategoryOrCityChanged :: Id DMOC.MerchantOperatingCity -> Maybe DPlan.DriverPlan -> Maybe Vehicle -> (Bool, Bool)
isPlanVehCategoryOrCityChanged opCityId mbDPlan mbVehicle = do
  let isVehicleCategoryChanged = ((mbVehicle >>= (.category)) /= (mbDPlan >>= (.vehicleCategory))) && isJust mbDPlan
      isCityChanged = maybe False (opCityId /=) (mbDPlan <&> (.merchantOpCityId))
  (isVehicleCategoryChanged, isCityChanged)

data DriverSpecificSubscriptionData = DriverSpecificSubscriptionData
  { isOnFreeTrial :: Bool,
    planMandatoryForCategory :: Bool,
    freeTrialDaysLeft :: Int,
    mbDriverPlan :: Maybe DPlan.DriverPlan,
    autoPayStatus :: Maybe DriverInfo.DriverAutoPayStatus,
    isEnabledForCategory :: Bool,
    isSubscriptionVehicleCategoryChanged :: Bool,
    isSubscriptionCityChanged :: Bool,
    isSubscriptionEnabledAtCategoryLevel :: Bool,
    freeTrialDays :: Int,
    freeTrialRides :: Int,
    totalRidesTaken :: Maybe Int,
    subscriptionDown :: Maybe Bool
  }
  deriving (Generic, Show, Eq, Ord)

getDriverSpecificSubscriptionDataWithSubsConfig ::
  (EsqDBFlow m r, CacheFlow m r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  TransporterConfig ->
  DriverInformation ->
  Maybe Vehicle ->
  m DriverSpecificSubscriptionData
getDriverSpecificSubscriptionDataWithSubsConfig (personId, _, opCityId) transporterConfig driverInfo mbVehicle = do
  let mbVehicleCategory = mbVehicle >>= (.category)
  subscriptionConfig <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCityId YATRI_SUBSCRIPTION
  (autoPayStatus, mbDriverPlan) <- DAPlan.getSubcriptionStatusWithPlan Plan.YATRI_SUBSCRIPTION personId
  freeTrialDaysLeft <- getFreeTrialDaysLeft transporterConfig.freeTrialDays driverInfo
  let freeTrialDays = transporterConfig.freeTrialDays
      freeTrialRides = fromMaybe 0 $ subscriptionConfig >>= (.numberOfFreeTrialRides)
      isSubscriptionEnabledAtCategoryLevel = fromMaybe False $ subscriptionConfig <&> (.isSubscriptionEnabledAtCategoryLevel)
      (isSubscriptionVehicleCategoryChanged, isSubscriptionCityChanged) = isPlanVehCategoryOrCityChanged opCityId mbDriverPlan mbVehicle
  (isOnFreeTrial, totalRidesTaken, subscriptionDown) <- do
    case subscriptionConfig of
      Just subsConfig -> do
        (ft, numRides) <- DAPlan.isOnFreeTrial personId subsConfig freeTrialDaysLeft mbDriverPlan
        return (ft, numRides, subsConfig.subscriptionDown)
      Nothing -> return (True, Nothing, Nothing)
  let planMandatoryForCategory = maybe False (\vcList -> isJust $ DL.find (\enabledVc -> maybe False (enabledVc ==) mbVehicleCategory) vcList) (subscriptionConfig >>= (.executionEnabledForVehicleCategories))
      isEnabledForCategory = maybe False (\vcList -> isJust $ DL.find (\enabledVc -> maybe False (enabledVc ==) mbVehicleCategory) vcList) (subscriptionConfig >>= (.subscriptionEnabledForVehicleCategories))
  return $ DriverSpecificSubscriptionData {..}
