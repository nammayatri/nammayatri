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
    ScheduleBooking (..),
    BookingAPIEntity (..),
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
    GetConsentReq (..),
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
    getStatsAllTime,
    getEarnings,
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
    getScheduledBookings,
    buildBookingAPIEntityFromBooking,
    mkBreakupItem,
    getInformationV2,
    clearDriverFeeWithCreate,
    verifyVpaStatus,
    getSecurityDepositDfStatus,
    refundByPayoutDriverFee,
    mkPayoutLockKeyByDriverAndService,
    consentResponse,
    findOnboardedDriversOrFleets,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Driver as DCommon
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
import qualified Data.HashMap.Strict as HM
import Data.List (intersect, nub, (\\))
import qualified Data.List as DL
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Tuple.Extra as DTE
import Domain.Action.Beckn.Search
import Domain.Action.Dashboard.Driver.Notification as DriverNotify (triggerDummyRideRequest)
import qualified Domain.Action.Internal.DriverMode as DDriverMode
import qualified Domain.Action.Internal.ProcessingChangeOnline as DOnlineDuration
import qualified Domain.Action.UI.DriverGoHomeRequest as DDGR
import qualified Domain.Action.UI.DriverHomeLocation as DDHL
import Domain.Action.UI.DriverOnboarding.AadhaarVerification (fetchAndCacheAadhaarImage)
import qualified Domain.Action.UI.DriverOnboardingV2 as DOV
import qualified Domain.Action.UI.DriverReferral as DUR
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
import qualified Domain.Types.ConditionalCharges as DCC
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
import Domain.Types.Estimate
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as Fare
import Domain.Types.FarePolicy (DriverExtraFeeBounds (..))
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.FleetDriverAssociation as FDA
import qualified Domain.Types.Invoice as Domain
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DTM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
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
import Domain.Types.VehicleRegistrationCertificate
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
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
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
import qualified Lib.Yudhishthira.Flow.Dashboard as Yudhishthira
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types as Yudhishthira
import SharedLogic.Allocator (AllocatorJobType (..), ScheduledRideAssignedOnUpdateJobData (..))
import qualified SharedLogic.Analytics as Analytics
import qualified SharedLogic.BehaviourManagement.CancellationRate as SCR
import SharedLogic.Booking
import SharedLogic.Cac
import SharedLogic.CallBAP (sendDriverOffer, sendRideAssignedUpdateToBAP)
import qualified SharedLogic.CallInternalMLPricing as ML
import qualified SharedLogic.DeleteDriver as DeleteDriverOnCheck
import qualified SharedLogic.DriverFee as SLDriverFee
import SharedLogic.DriverOnboarding
import SharedLogic.DriverPool as DP
import qualified SharedLogic.EventTracking as ET
import qualified SharedLogic.External.LocationTrackingService.Flow as LTF
import SharedLogic.FareCalculator
import qualified SharedLogic.FareCalculatorV2 as FCV2
import SharedLogic.FarePolicy
import qualified SharedLogic.Merchant as SMerchant
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.Payment as SPayment
import SharedLogic.Pricing
import SharedLogic.Ride
import qualified SharedLogic.SearchTryLocker as CS
import qualified SharedLogic.Type as SLT
import SharedLogic.VehicleServiceTier
import qualified Storage.Cac.DriverPoolConfig as SCDPC
import qualified Storage.Cac.GoHomeConfig as CGHC
import qualified Storage.Cac.TransporterConfig as CTC
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
import qualified Storage.Clickhouse.DailyStats as CHDS
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingExtra as QBE
import qualified Storage.Queries.DailyStats as SQDS
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverGoHomeRequest as QDGR
import qualified Storage.Queries.DriverHomeLocation as QDHL
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverOperatorAssociationExtra as QDOA
import qualified Storage.Queries.DriverPlan as QDriverPlan
import qualified Storage.Queries.DriverQuote as QDrQt
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
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
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import qualified Storage.Queries.VendorFee as QVF
import qualified Tools.Auth as Auth
import Tools.Error
import Tools.Event
import qualified Tools.Payout as Payout
import Tools.SMS as Sms hiding (Success)
import Tools.Verification hiding (ImageType, length)
import Utils.Common.Cac.KeyNameConstants

data FleetInfo = FleetInfo
  { id :: Text,
    ownerName :: Text,
    fleetName :: Maybe Text,
    phoneNumber :: Maybe Text,
    address :: Maybe Address,
    requestReason :: Maybe Text,
    responseReason :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

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
    dynamicReferralCode :: Maybe Text,
    operatorReferralCode :: Maybe Text,
    organization :: DM.MerchantAPIEntity,
    language :: Maybe Maps.Language,
    alternateNumber :: Maybe Text,
    canDowngradeToSedan :: Bool,
    canDowngradeToHatchback :: Bool,
    canDowngradeToTaxi :: Bool,
    canSwitchToRental :: Bool,
    canSwitchToInterCity :: Bool,
    canSwitchToIntraCity :: Bool,
    isPetModeEnabled :: Bool,
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
    assignedRidesCountDaily :: Maybe Int,
    cancelledRidesCountDaily :: Maybe Int,
    assignedRidesCountWeekly :: Maybe Int,
    cancelledRidesCountWeekly :: Maybe Int,
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
    subscriptionDown :: Maybe Bool,
    qrUrl :: Maybe Text,
    driverTags :: Maybe DA.Value,
    nyClubConsent :: Maybe Bool,
    cancellationRateSlabConfig :: Maybe Domain.Types.TransporterConfig.CancellationRateSlabConfig,
    enabledAt :: Maybe UTCTime,
    fleetOwnerId :: Maybe Text, -- deprecate later
    operatorId :: Maybe Text,
    fleetRequest :: Maybe FleetInfo,
    tripDistanceMaxThreshold :: Maybe Meters,
    tripDistanceMinThreshold :: Maybe Meters,
    maxPickupRadius :: Maybe Meters,
    isSilentModeEnabled :: Maybe Bool,
    reactVersion :: Maybe Text,
    rideRequestVolume :: Maybe Int,
    isTTSEnabled :: Maybe Bool,
    isHighAccuracyLocationEnabled :: Maybe Bool,
    rideRequestVolumeEnabled :: Maybe Bool,
    profilePhotoUploadedAt :: Maybe UTCTime,
    activeFleet :: Maybe FleetInfo,
    onboardingAs :: Maybe DriverInfo.OnboardingAs,
    vehicleImageUploadedAt :: Maybe UTCTime
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
    isPetModeEnabled :: Bool,
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
    subscriptionDown :: Maybe Bool,
    qrUrl :: Maybe Text,
    driverTags :: Maybe DA.Value,
    nyClubConsent :: Maybe Bool,
    enabledAt :: Maybe UTCTime,
    tripDistanceMaxThreshold :: Maybe Meters,
    tripDistanceMinThreshold :: Maybe Meters,
    maxPickupRadius :: Maybe Meters,
    isSilentModeEnabled :: Maybe Bool,
    reactVersion :: Maybe Text,
    rideRequestVolume :: Maybe Int,
    isTTSEnabled :: Maybe Bool,
    isHighAccuracyLocationEnabled :: Maybe Bool,
    rideRequestVolumeEnabled :: Maybe Bool,
    profilePhotoUploadedAt :: Maybe UTCTime,
    vehicleImageUploadedAt :: Maybe UTCTime
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
    isPetModeEnabled :: Maybe Bool,
    clientVersion :: Maybe Version,
    bundleVersion :: Maybe Version,
    gender :: Maybe SP.Gender,
    languagesSpoken :: Maybe [Text],
    hometown :: Maybe Text,
    vehicleName :: Maybe Text,
    availableUpiApps :: Maybe Text,
    tripDistanceMaxThreshold :: Maybe Meters,
    tripDistanceMinThreshold :: Maybe Meters,
    maxPickupRadius :: Maybe Meters,
    isSilentModeEnabled :: Maybe Bool,
    rideRequestVolume :: Maybe Int,
    reactVersion :: Maybe Text,
    isTTSEnabled :: Maybe Bool,
    isHighAccuracyLocationEnabled :: Maybe Bool,
    rideRequestVolumeEnabled :: Maybe Bool,
    onboardingAs :: Maybe DriverInfo.OnboardingAs
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
    billingCategory :: SLT.BillingCategory,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    stopLocationId :: Maybe (Id DLoc.Location),
    roundTrip :: Maybe Bool,
    returnTime :: Maybe UTCTime,
    distanceToPickup :: Maybe Meters,
    isScheduled :: Bool,
    coinsRewardedOnGoldTierRide :: Maybe Int,
    isInsured :: Maybe Bool,
    insuredAmount :: Maybe Text,
    paymentInstrument :: Maybe DMPM.PaymentInstrument
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
    brisqueFeatures :: [Double],
    imageType :: Maybe ImageType,
    rcNo :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ImageType = ProfilePhoto | QrImage | VehiclePhoto
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

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

data GetConsentReq = GetConsentReq
  { consent :: Bool
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
    HasField "s3Env" r (S3.S3Env m),
    HasField "cloudType" r (Maybe CloudType)
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Plan.ServiceNames ->
  Maybe Bool ->
  UpdateProfileInfoPoints ->
  m DriverInformationRes
getInformationV2 (personId, merchantId, merchantOpCityId) mbClientId toss tenant' context mbServiceName mbFleetInfo req = do
  driverInfo <- QDriverInformation.findById personId >>= fromMaybeM DriverInfoNotFound
  whenJust req.isAdvancedBookingEnabled $ \isAdvancedBookingEnabled ->
    unless (driverInfo.forwardBatchingEnabled == isAdvancedBookingEnabled) $
      QDriverInformation.updateForwardBatchingEnabled isAdvancedBookingEnabled personId
  whenJust req.isInteroperable $ \isInteroperable ->
    unless (driverInfo.isInteroperable == isInteroperable) $
      QDriverInformation.updateIsInteroperable isInteroperable personId
  whenJust req.isCategoryLevelSubscriptionEnabled $ \isCategoryLevelSubscriptionEnabled ->
    QDriverPlan.updateIsSubscriptionEnabledAtCategoryLevel personId YATRI_SUBSCRIPTION isCategoryLevelSubscriptionEnabled
  getInformation (personId, merchantId, merchantOpCityId) mbClientId toss tenant' context mbServiceName (Just driverInfo) mbFleetInfo

getInformation ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CacheFlow m r,
    HasField "s3Env" r (S3.S3Env m),
    HasField "cloudType" r (Maybe CloudType)
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Plan.ServiceNames ->
  Maybe DriverInformation ->
  Maybe Bool ->
  m DriverInformationRes
getInformation (personId, merchantId, merchantOpCityId) mbClientId toss tnant' context mbServiceName mbDriverInfo mbFleetInfo = do
  let driverId = cast personId
      serviceName = fromMaybe Plan.YATRI_SUBSCRIPTION mbServiceName
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  when (isNothing person.clientId && isJust mbClientId) $ QPerson.updateClientId mbClientId person.id
  cloudType <- asks (.cloudType)
  when (person.cloudType /= cloudType) $ QPerson.updateCloudType cloudType person.id
  driverStats <- runInReplica $ QDriverStats.findById driverId >>= fromMaybeM DriverInfoNotFound
  driverInfo <- maybe (QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound) return mbDriverInfo
  driverReferralCode <- QDR.findById (cast driverId)
  operatorReferral <- case driverInfo.referredByOperatorId of
    Just opId -> QDR.findById (cast (Id opId))
    Nothing -> pure Nothing
  driverEntity <- buildDriverEntityRes (person, driverInfo, driverStats, merchantOpCityId) serviceName
  dues <- QDF.findAllFeeByTypeServiceStatusAndDriver serviceName driverId [DDF.RECURRING_INVOICE, DDF.RECURRING_EXECUTION_INVOICE] [DDF.PAYMENT_PENDING, DDF.PAYMENT_OVERDUE]
  let currentDues = sum $ map (\dueInvoice -> SLDriverFee.roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) dues
  let manualDues = sum $ map (\dueInvoice -> SLDriverFee.roundToHalf dueInvoice.currency (dueInvoice.govtCharges + dueInvoice.platformFee.fee + dueInvoice.platformFee.cgst + dueInvoice.platformFee.sgst)) $ filter (\due -> due.status == DDF.PAYMENT_OVERDUE) dues
  logDebug $ "alternateNumber-" <> show driverEntity.alternateNumber
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (.useCACForFrontend) systemConfigs
  allFdas <- QFDA.findAllByDriverIdWithStatus driverId
  let activeFda = find (.isActive) allFdas
      inactiveFda = find (not . (.isActive)) allFdas
  doa <- QDOA.findByDriverId driverId True
  let context' = fromMaybe DAKM.empty (DA.decode $ BSL.pack $ T.unpack $ fromMaybe "{}" context)
  frntndfgs <- if useCACConfig then getFrontendConfigs merchantOpCityId toss tnant' context' else return Nothing
  let mbMd5Digest = T.pack . show . MD5.md5 . DA.encode <$> frntndfgs
  merchant <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  driverGoHomeInfo <- CQDGR.getDriverGoHomeRequestInfo driverId merchantOpCityId Nothing
  makeDriverInformationRes merchantOpCityId driverEntity driverInfo merchant driverReferralCode driverStats driverGoHomeInfo (Just currentDues) (Just manualDues) mbMd5Digest operatorReferral ((.operatorId) <$> doa) inactiveFda activeFda mbFleetInfo

setActivity :: (CacheFlow m r, EsqDBFlow m r, HasField "serviceClickhouseCfg" r CH.ClickhouseCfg, HasField "serviceClickhouseEnv" r CH.ClickhouseEnv) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Bool -> Maybe DriverInfo.DriverMode -> m APISuccess.APISuccess
setActivity (personId, merchantId, merchantOpCityId) isActive mode = do
  isLocked <- withLockDriverIdForSetActivity personId
  unless isLocked $ throwError $ DriverActivityUpdateInProgress personId.getId
  finally
    ( do
        void $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
        let driverId = cast personId
        driverInfo <- QDriverInformation.findById driverId >>= fromMaybeM DriverInfoNotFound
        transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
        when (isActive || (isJust mode && (mode == Just DriverInfo.SILENT || mode == Just DriverInfo.ONLINE))) $ do
          merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
          mbVehicle <- QVehicle.findById personId
          DriverSpecificSubscriptionData {..} <- getDriverSpecificSubscriptionDataWithSubsConfig (personId, merchantId, merchantOpCityId) transporterConfig driverInfo mbVehicle Plan.YATRI_SUBSCRIPTION
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
          let isVehicleVariantDisabledForSubscription = maybe False (`elem` (fromMaybe [] vehicleVariantsDisabledForSubscription)) (mbVehicle <&> (.variant))
          when ((planBasedChecks || changeBasedChecks) && (not isVehicleVariantDisabledForSubscription)) $ throwError (NoPlanSelected personId.getId)
          when merchant.onlinePayment $ do
            driverBankAccount <-
              QFDA.findByDriverId driverId True >>= \case
                Just fleetDriverAssociation -> QDBA.findByPrimaryKey (Id @SP.Person fleetDriverAssociation.fleetOwnerId) >>= fromMaybeM (DriverBankAccountNotFound driverId.getId)
                Nothing -> QDBA.findByPrimaryKey driverId >>= fromMaybeM (DriverBankAccountNotFound driverId.getId)
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
        when (driverInfo.active /= isActive || driverInfo.mode /= mode) $ do
          let newFlowStatus = DDriverMode.getDriverFlowStatus (mode <|> Just DriverInfo.OFFLINE) isActive
          -- Track offline timestamp when driver goes offline
          if (mode == Just DriverInfo.OFFLINE && driverInfo.mode /= Just DriverInfo.OFFLINE)
            then do
              now <- getCurrentTime
              logInfo $ "Driver going OFFLINE at: " <> show now <> " for driverId: " <> show driverId
              DDriverMode.updateDriverModeAndFlowStatus driverId transporterConfig isActive (mode <|> Just DriverInfo.OFFLINE) newFlowStatus driverInfo Nothing (Just now)
            else DDriverMode.updateDriverModeAndFlowStatus driverId transporterConfig isActive (mode <|> Just DriverInfo.OFFLINE) newFlowStatus driverInfo Nothing Nothing
        pure APISuccess.Success
    )
    ( do
        Redis.unlockRedis (buildSetActivityLockKey personId)
    )
  where
    withLockDriverIdForSetActivity driverId' = do
      isLockSuccessful <- Redis.tryLockRedis (buildSetActivityLockKey driverId') 5
      return isLockSuccessful

    buildSetActivityLockKey :: Id SP.Person -> Text
    buildSetActivityLockKey driverId' = "Driver:SetActivity:" <> show driverId'

activateGoHomeFeature :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id DDHL.DriverHomeLocation -> LatLong -> Flow APISuccess.APISuccess
activateGoHomeFeature (driverId, merchantId, merchantOpCityId) driverHomeLocationId driverLocation = do
  isLocked <- withLockDriverId driverId
  if isLocked
    then do
      finally
        ( do
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
            unless (dghInfo.cnt > 0) $ throwError DriverGoHomeRequestDailyUsageLimitReached
            unlessM (checkIfGoToInDifferentGeometry merchant driverLocation homePos) $ throwError CannotEnableGoHomeForDifferentCity
            whenM (fmap ((dghInfo.status == Just DDGR.ACTIVE) ||) (isJust <$> QDGR.findActive driverId)) $ throwError DriverGoHomeRequestAlreadyActive
            activateDriverGoHomeRequest merchantId merchantOpCityId driverId driverHomeLocation goHomeConfig dghInfo
            pure ()
        )
        ( do
            Redis.unlockRedis (buildActivateGoHomeKey driverId)
        )
    else throwError GoHomeRequestInProgress
  pure APISuccess.Success
  where
    checkIfGoToInDifferentGeometry :: DM.Merchant -> LatLong -> LatLong -> Flow Bool
    checkIfGoToInDifferentGeometry merchant driverLoc = uncurry (liftM2 (\dl hl -> dl == hl && dl /= Context.City "AnyCity" && hl /= Context.City "AnyCity")) . DTE.both ((((.city) . (.nearestOperatingCity)) <$>) . runInReplica . getNearestOperatingAndSourceCity merchant) . (driverLoc,)

    withLockDriverId driverId' = do
      isLockSuccussful <- Redis.tryLockRedis (buildActivateGoHomeKey driverId') 30
      return isLockSuccussful

    buildActivateGoHomeKey :: Id SP.Person -> Text
    buildActivateGoHomeKey driverId' = "Driver:GoHome:Activate:" <> show driverId'

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
  currentRide <- Ride.findNewOrInProgressRideByGHRId ghrId
  when (isJust currentRide) $ throwError $ DriverGoHomeRequestRideInProgress
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

buildDriverEntityRes :: (EsqDBReplicaFlow m r, EncFlow m r, CacheFlow m r, HasField "s3Env" r (S3.S3Env m), EsqDBFlow m r) => (SP.Person, DriverInformation, DStats.DriverStats, Id DMOC.MerchantOperatingCity) -> Plan.ServiceNames -> m DriverEntityRes
buildDriverEntityRes (person, driverInfo, driverStats, merchantOpCityId) serviceName = do
  transporterConfig <- SCTC.findByMerchantOpCityId person.merchantOperatingCityId (Just (DriverId (cast person.id))) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  vehicleMB <- QVehicle.findById person.id
  DriverSpecificSubscriptionData {mbDriverPlan = driverPlan, ..} <- getDriverSpecificSubscriptionDataWithSubsConfig (person.id, transporterConfig.merchantId, merchantOpCityId) transporterConfig driverInfo vehicleMB serviceName
  now <- getCurrentTime
  decMobNum <- mapM decrypt person.mobileNumber
  decaltMobNum <- mapM decrypt person.alternateMobileNumber
  let maskedDeviceToken = maskText . (.getFCMRecipientToken) <$> person.deviceToken
  (mediaUrl, profilePhotoUploadedAt) <- case person.faceImageId of
    Just mediaId -> do
      mediaEntry <- runInReplica $ MFQuery.findById mediaId >>= fromMaybeM (FileDoNotExist mediaId.getId)
      return (Just mediaEntry.url, Just mediaEntry.createdAt)
    Nothing -> return (Nothing, driverInfo.enabledAt)
  vehicleImageUploadedAt <- case vehicleMB of
    Just vehicle -> case vehicle.vehicleImageId of
      Just vehicleImageId -> do
        mediaEntry <- runInReplica $ MFQuery.findById vehicleImageId >>= fromMaybeM (FileDoNotExist vehicleImageId.getId)
        return (Just mediaEntry.createdAt)
      Nothing -> return Nothing
    Nothing -> return Nothing
  aadhaarCardPhotoResp <- withTryCatch "fetchAndCacheAadhaarImage:buildDriverEntityRes" (fetchAndCacheAadhaarImage person driverInfo)
  let aadhaarCardPhoto = join (eitherToMaybe aadhaarCardPhotoResp)
  let rating =
        if transporterConfig.ratingAsDecimal
          then SP.roundToOneDecimal <$> driverStats.rating
          else driverStats.rating <&> (\(Centesimal x) -> Centesimal (fromInteger (round x)))
  qrUrl <- forM person.qrImageId $ \mediaId -> do
    mediaEntry <- runInReplica $ MFQuery.findById mediaId >>= fromMaybeM (FileDoNotExist person.id.getId)
    return mediaEntry.url
  supportedServiceTiers <- CQFP.findSupportedServiceTiersByMerchantOpCityId person.merchantOperatingCityId
  (checkIfACWorking, mbDefaultServiceTier, isVehicleSupported) <-
    case vehicleMB of
      Nothing -> return (False, Nothing, False)
      Just vehicle -> do
        cityServiceTiers <- CQVST.findAllByMerchantOpCityId person.merchantOperatingCityId Nothing
        let allVehicleSupportedDefaultServiceTiers = sortOn (fmap Down . (.airConditionedThreshold)) $ (filter (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant && vst.serviceTierType `elem` supportedServiceTiers) cityServiceTiers)
        let isVehicleSupported = not $ null allVehicleSupportedDefaultServiceTiers
        let mbDefaultServiceTierItem =
              if (null allVehicleSupportedDefaultServiceTiers)
                then find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) cityServiceTiers
                else listToMaybe allVehicleSupportedDefaultServiceTiers
        let checIfACWorking' =
              case mbDefaultServiceTierItem >>= (.airConditionedThreshold) of
                Nothing -> False
                Just acThreshold -> do
                  (fromMaybe 0 driverInfo.airConditionScore) <= acThreshold
                    && maybe True (\lastCheckedAt -> fromInteger (diffDays (utctDay now) (utctDay lastCheckedAt)) >= transporterConfig.acStatusCheckGap) driverInfo.lastACStatusCheckedAt
        return (checIfACWorking', (.serviceTierType) <$> mbDefaultServiceTierItem, isVehicleSupported)
  onRideFlag <-
    if driverInfo.onRide && driverInfo.onboardingVehicleCategory /= Just DVC.BUS
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
      mbOverchargingBlockedTill = Yudhishthira.accessTagKey (LYT.TagName "OverchargingBlockedTill") driverTags
  currentLocalTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let (overchargingBlocked, blockedTill, driverOverchargingTag) =
        case (mbDriverOverchargingTag, mbOverchargingBlockedTill) of
          (Just driverOverchargingTag', Just overchargingBlockedTillV) | transporterConfig.enableOverchargingBlocker -> do
            let bt :: DA.Result Integer = DA.fromJSON overchargingBlockedTillV
            case bt of
              DA.Success bt' -> do
                let bt'' = epochToUTCTime (bt' - fromIntegral transporterConfig.timeDiffFromUtc)
                let isInBlockedTimeRange = currentLocalTime < bt''
                let highOC = highOverchargingTag driverOverchargingTag' && isInBlockedTimeRange
                if highOC
                  then (highOC, Just bt'', Just driverOverchargingTag')
                  else (False, Nothing, Nothing)
              _ -> (False, Nothing, Nothing)
          _ -> (False, Nothing, Nothing)
  return $
    DriverEntityRes
      { id = person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        mobileNumber = decMobNum,
        email = person.email,
        linkedVehicle = makeVehicleAPIEntity mbDefaultServiceTier <$> vehicleMB,
        active = driverInfo.active,
        onRide = onRideFlag,
        enabled = driverInfo.enabled,
        isPetModeEnabled = driverInfo.isPetModeEnabled,
        blocked = driverInfo.blocked || overchargingBlocked,
        blockExpiryTime = driverInfo.blockExpiryTime <|> blockedTill,
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
        reactVersion = person.reactBundleVersion,
        gender = Just person.gender,
        payoutVpa = driverInfo.payoutVpa,
        payoutVpaStatus = driverInfo.payoutVpaStatus,
        payoutVpaBankAccount = driverInfo.payoutVpaBankAccount,
        subscriptionEnabledForVehicleCategory = isEnabledForCategory,
        isSpecialLocWarrior = driverInfo.isSpecialLocWarrior,
        safetyTag = mbDriverSafetyTag,
        safetyScore = mbDriverSafetyScore,
        overchargingTag = driverOverchargingTag,
        ridesWithFareIssues = mbRidesWithFareIssues,
        totalRidesConsideredForFareIssues = mbTotalRidesConsideredForFareIssues,
        softBlockStiers = driverInfo.softBlockStiers,
        softBlockExpiryTime = driverInfo.softBlockExpiryTime,
        softBlockReasonFlag = driverInfo.softBlockReasonFlag,
        onboardingVehicleCategory = driverInfo.onboardingVehicleCategory,
        qrUrl,
        driverTags = Just driverTags,
        nyClubConsent = person.nyClubConsent,
        enabledAt = driverInfo.enabledAt,
        tripDistanceMaxThreshold = driverInfo.tripDistanceMaxThreshold,
        tripDistanceMinThreshold = driverInfo.tripDistanceMinThreshold,
        maxPickupRadius = driverInfo.maxPickupRadius,
        isSilentModeEnabled = driverInfo.isSilentModeEnabled,
        rideRequestVolume = driverInfo.rideRequestVolume,
        isTTSEnabled = driverInfo.isTTSEnabled,
        isHighAccuracyLocationEnabled = driverInfo.isHighAccuracyLocationEnabled,
        rideRequestVolumeEnabled = driverInfo.rideRequestVolumeEnabled,
        ..
      }
  where
    highOverchargingTag overchargingTag = overchargingTag `elem` ["SuperOverCharging", "HighOverCharging", "MediumOverCharginge"]

    epochToUTCTime epoch = posixSecondsToUTCTime (fromInteger epoch)

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
    HasField "version" r DeploymentVersion,
    HasField "cloudType" r (Maybe CloudType)
  ) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  UpdateDriverReq ->
  m UpdateDriverRes
updateDriver (personId, _, merchantOpCityId) mbBundleVersion mbClientVersion mbConfigVersion mbReactBundleVersion mbDevice req = do
  runRequestValidation validateUpdateDriverReq req
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  deploymentVersion <- asks (.version)
  cloudType <- asks (.cloudType)
  let updPerson =
        person{firstName = fromMaybe person.firstName req.firstName,
               middleName = req.middleName <|> person.middleName,
               lastName = req.lastName <|> person.lastName,
               deviceToken = req.deviceToken <|> person.deviceToken,
               language = req.language <|> person.language,
               clientSdkVersion = mbClientVersion <|> person.clientSdkVersion,
               clientBundleVersion = mbBundleVersion <|> person.clientBundleVersion,
               reactBundleVersion = mbReactBundleVersion <|> person.reactBundleVersion,
               clientConfigVersion = mbConfigVersion <|> person.clientConfigVersion,
               clientDevice = getDeviceFromText mbDevice <|> person.clientDevice,
               backendConfigVersion = person.backendConfigVersion,
               backendAppVersion = Just deploymentVersion.getDeploymentVersion,
               gender = fromMaybe person.gender req.gender,
               hometown = req.hometown <|> person.hometown,
               languagesSpoken = req.languagesSpoken <|> person.languagesSpoken,
               cloudType = cloudType
              }
  mVehicle <- QVehicle.findById personId
  driverInfo <- QDriverInformation.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  let isPetModeEnabled = fromMaybe driverInfo.isPetModeEnabled req.isPetModeEnabled
      tripDistanceMaxThreshold = case req.tripDistanceMaxThreshold of
        Nothing -> driverInfo.tripDistanceMaxThreshold
        Just val -> if val.getMeters < 0 then Nothing else Just val
      tripDistanceMinThreshold = case req.tripDistanceMinThreshold of
        Nothing -> driverInfo.tripDistanceMinThreshold
        Just val -> if val.getMeters < 0 then Nothing else Just val
      maxPickupRadius = case req.maxPickupRadius of
        Nothing -> driverInfo.maxPickupRadius
        Just val -> if val.getMeters < 0 then Nothing else Just val
      isSilentModeEnabled = req.isSilentModeEnabled <|> driverInfo.isSilentModeEnabled
      rideRequestVolume = req.rideRequestVolume <|> driverInfo.rideRequestVolume
      isTTSEnabled = req.isTTSEnabled <|> driverInfo.isTTSEnabled
      isHighAccuracyLocationEnabled = req.isHighAccuracyLocationEnabled <|> driverInfo.isHighAccuracyLocationEnabled
      rideRequestVolumeEnabled = req.rideRequestVolumeEnabled <|> driverInfo.rideRequestVolumeEnabled
      onboardingAs = req.onboardingAs <|> driverInfo.onboardingAs
  whenJust mVehicle $ \vehicle -> do
    when (isJust req.canDowngradeToSedan || isJust req.canDowngradeToHatchback || isJust req.canDowngradeToTaxi || isJust req.canSwitchToRental || isJust req.canSwitchToInterCity || isJust req.isPetModeEnabled || isJust req.tripDistanceMaxThreshold || isJust req.tripDistanceMinThreshold || isJust req.maxPickupRadius || isJust req.isSilentModeEnabled || isJust req.rideRequestVolume || isJust req.isTTSEnabled || isJust req.isHighAccuracyLocationEnabled || isJust req.rideRequestVolumeEnabled) $ do
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
              DV.BOAT -> [DVST.BOAT]
              DV.AUTO_PLUS -> [DVST.AUTO_PLUS]
              DV.VIP_ESCORT -> [DVST.VIP_ESCORT]
              DV.VIP_OFFICER -> [DVST.VIP_OFFICER]
              DV.AC_PRIORITY -> [DVST.AC_PRIORITY]
              DV.BIKE_PLUS -> [DVST.BIKE_PLUS]
              DV.E_RICKSHAW -> [DVST.E_RICKSHAW]
              DV.AUTO_LITE -> [DVST.AUTO_LITE]
              DV.PINK_AUTO -> [DVST.PINK_AUTO]

      QDriverInformation.updateDriverInformation canDowngradeToSedan canDowngradeToHatchback canDowngradeToTaxi canSwitchToRental canSwitchToInterCity canSwitchToIntraCity availableUpiApps isPetModeEnabled tripDistanceMaxThreshold tripDistanceMinThreshold maxPickupRadius isSilentModeEnabled rideRequestVolume isTTSEnabled isHighAccuracyLocationEnabled rideRequestVolumeEnabled onboardingAs person.id
      when (isJust req.canDowngradeToSedan || isJust req.canDowngradeToHatchback || isJust req.canDowngradeToTaxi) $
        QVehicle.updateSelectedServiceTiers selectedServiceTiers person.id

    let petTag = Yudhishthira.TagNameValue "PetDriver#\"true\""
    when (isPetModeEnabled && maybe False (Yudhishthira.elemTagNameValue petTag) person.driverTag) $
      logInfo "Tag already exists, update expiry"
    tag <-
      if isPetModeEnabled
        then do
          mbNammTag <- Yudhishthira.verifyTag petTag
          now <- getCurrentTime
          let reqDriverTagWithExpiry = Yudhishthira.addTagExpiry petTag (mbNammTag >>= (.validity)) now
          return $ Yudhishthira.replaceTagNameValue person.driverTag reqDriverTagWithExpiry
        else return $ Yudhishthira.removeTagName person.driverTag petTag
    unless (Just (Yudhishthira.showRawTags tag) == (Yudhishthira.showRawTags <$> person.driverTag)) $
      QPerson.updateDriverTag (Just tag) personId

  updatedDriverInfo <- QDriverInformation.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  when (isJust req.vehicleName) $ QVehicle.updateVehicleName req.vehicleName personId
  QPerson.updatePersonRec personId updPerson
  driverStats <- runInReplica $ QDriverStats.findById (cast personId) >>= fromMaybeM DriverInfoNotFound
  driverEntity <- buildDriverEntityRes (updPerson, updatedDriverInfo, driverStats, merchantOpCityId) Plan.YATRI_SUBSCRIPTION
  driverReferralCode <- QDR.findById personId
  allFdas <- QFDA.findAllByDriverIdWithStatus personId
  let activeFda = find (.isActive) allFdas
      inactiveFda = find (not . (.isActive)) allFdas
  doa <- QDOA.findByDriverId personId True
  operatorReferral <- case updatedDriverInfo.referredByOperatorId of
    Just opId -> QDR.findById (cast (Id opId))
    Nothing -> pure Nothing
  let merchantId = person.merchantId
  org <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  driverGoHomeInfo <- CQDGR.getDriverGoHomeRequestInfo personId merchantOpCityId Nothing
  makeDriverInformationRes merchantOpCityId driverEntity updatedDriverInfo org driverReferralCode driverStats driverGoHomeInfo Nothing Nothing Nothing operatorReferral ((.operatorId) <$> doa) inactiveFda activeFda Nothing
  where
    -- logic is deprecated, should be handle from driver service tier options now, kept it for backward compatibility
    checkIfCanDowngrade vehicle = do
      when
        ( (vehicle.variant == DV.AUTO_RICKSHAW || vehicle.variant == DV.AUTO_PLUS || vehicle.variant == DV.TAXI || vehicle.variant == DV.HATCHBACK || vehicle.variant == DV.AUTO_LITE || vehicle.variant == DV.PINK_AUTO)
            && (req.canDowngradeToSedan == Just True || req.canDowngradeToHatchback == Just True)
        )
        $ throwError $ InvalidRequest $ "Can't downgrade from " <> (show vehicle.variant)
      when (vehicle.variant == DV.SUV && req.canDowngradeToTaxi == Just True) $
        throwError $ InvalidRequest $ "Can't downgrade to NON-AC TAXI from " <> (show vehicle.variant)
      when
        ( (vehicle.variant == DV.AUTO_RICKSHAW || vehicle.variant == DV.AUTO_PLUS || vehicle.variant == DV.TAXI || vehicle.variant == DV.AUTO_LITE || vehicle.variant == DV.PINK_AUTO)
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

buildFleetInfo :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => SP.Person -> FDA.FleetDriverAssociation -> m FleetInfo
buildFleetInfo person fda = do
  fleetPhoneNumber <- decrypt `mapM` person.mobileNumber
  fleetOwnerInfo <- QFOI.findByPrimaryKey (Id fda.fleetOwnerId)
  return $
    FleetInfo
      { id = fda.fleetOwnerId,
        ownerName = person.firstName <> maybe "" (" " <>) person.lastName,
        fleetName = fleetOwnerInfo >>= (.fleetName),
        phoneNumber = fleetPhoneNumber,
        address = fleetOwnerInfo >>= (.stripeAddress),
        requestReason = fda.requestReason,
        responseReason = fda.responseReason,
        createdAt = fda.createdAt
      }

makeDriverInformationRes :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Id DMOC.MerchantOperatingCity -> DriverEntityRes -> DriverInformation -> DM.Merchant -> Maybe DR.DriverReferral -> DriverStats -> DDGR.CachedGoHomeRequest -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe Text -> Maybe DR.DriverReferral -> Maybe Text -> Maybe FDA.FleetDriverAssociation -> Maybe FDA.FleetDriverAssociation -> Maybe Bool -> m DriverInformationRes
makeDriverInformationRes merchantOpCityId DriverEntityRes {..} driverInfo merchant referralCode driverStats dghInfo currentDues manualDues md5DigestHash operatorReferral operatorId mbInactiveFda mbActiveFda mbFleetInfo = do
  (activeFleet, fleetRequest) <-
    if mbFleetInfo == Just True
      then do
        let fleetOwnerIds = catMaybes [(.fleetOwnerId) <$> mbActiveFda, (.fleetOwnerId) <$> mbInactiveFda]
        fleetOwners <- QPerson.findAllByPersonIds fleetOwnerIds

        activeFleetInfo <- case mbActiveFda of
          Just activeFda ->
            case find (\p -> p.id.getId == activeFda.fleetOwnerId) fleetOwners of
              Just person -> Just <$> buildFleetInfo person activeFda
              Nothing -> return Nothing
          Nothing -> return Nothing

        fleetRequestInfo <- case mbInactiveFda of
          Just inactiveFda ->
            case find (\p -> p.id.getId == inactiveFda.fleetOwnerId) fleetOwners of
              Just person -> Just <$> buildFleetInfo person inactiveFda
              Nothing -> return Nothing
          Nothing -> return Nothing

        return (activeFleetInfo, fleetRequestInfo)
      else return (Nothing, Nothing)
  merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist merchantOpCityId.getId)
  mbVehicle <- QVehicle.findById id
  let vehicleCategory = fromMaybe DVC.AUTO_CATEGORY ((.category) =<< mbVehicle)
  mbPayoutConfig <- CPC.findByPrimaryKey merchantOpCityId vehicleCategory Nothing
  cancellationRateData <- SCR.getCancellationRateData merchantOpCityId id
  merchantConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  bankDetails <-
    if merchant.onlinePayment
      then do
        mbDriverBankAccount <- QDBA.findByPrimaryKey id
        return $ mbDriverBankAccount <&> (\DOBA.DriverBankAccount {..} -> DOVT.BankAccountResp {paymentMode = fromMaybe DMPM.LIVE paymentMode, ..})
      else return Nothing
  (refCode, dynamicReferralCode) <-
    case referralCode of
      Nothing -> do
        res <- DUR.generateReferralCode (Just SP.DRIVER) (driverStats.driverId, merchant.id, merchantOpCityId)
        return (Just $ Id res.referralCode, res.dynamicReferralCode)
      Just drc -> do
        transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
        fmap (\drc' -> (Just drc'.referralCode, drc'.dynamicReferralCode)) $
          if transporterConfig.dynamicReferralCodeEnabled
            then DUR.checkAndUpdateDynamicReferralCode merchantOperatingCity.merchantId merchantOpCityId transporterConfig onRide drc
            else pure drc
  CGHC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast id))) >>= \cfg ->
    return $
      DriverInformationRes
        { organization = DM.makeMerchantAPIEntity merchant,
          referralCode = refCode <&> (.getId),
          dynamicReferralCode = dynamicReferralCode,
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
          assignedRidesCountDaily = (.assignedCountDaily) <$> cancellationRateData,
          cancelledRidesCountDaily = (.cancelledCountDaily) <$> cancellationRateData,
          assignedRidesCountWeekly = (.assignedCountWeekly) <$> cancellationRateData,
          cancelledRidesCountWeekly = (.cancelledCountWeekly) <$> cancellationRateData,
          cancellationRateSlabConfig = merchantConfig.cancellationRateSlabConfig,
          favCount = Just driverStats.favRiderCount,
          operatorReferralCode = (.referralCode.getId) <$> operatorReferral,
          activeFleet = activeFleet,
          fleetRequest = fleetRequest,
          fleetOwnerId = (.fleetOwnerId) <$> mbActiveFda,
          onboardingAs = driverInfo.onboardingAs,
          ..
        }

getNearbySearchRequests ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
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
      (estimate :: Maybe Estimate) <- runInReplica $ QEst.findById (Id searchTry.estimateId)
      bapMetadata <- CQSM.findBySubscriberIdAndDomain (Id searchRequest.bapId) Domain.MOBILITY
      isValueAddNP <- CQVAN.isValueAddNP searchRequest.bapId
      farePolicy <- getFarePolicyByEstOrQuoteId (Just $ Maps.getCoordinates searchRequest.fromLocation) (Just . Maps.getCoordinates =<< searchRequest.toLocation) searchRequest.fromLocGeohash searchRequest.toLocGeohash searchRequest.estimatedDistance searchRequest.estimatedDuration searchRequest.merchantOperatingCityId searchTry.tripCategory nearbyReq.vehicleServiceTier searchRequest.area (fromMaybe searchTry.estimateId nearbyReq.estimateId) Nothing Nothing searchRequest.dynamicPricingLogicVersion (Just (TransactionId (Id searchRequest.transactionId))) searchRequest.configInExperimentVersions searchRequest.specialLocationName
      popupDelaySeconds <- DP.getPopupDelay merchantOpCityId (cast driverId) cancellationRatio cancellationScoreRelatedConfig transporterConfig.defaultPopupDelay
      let useSilentFCMForForwardBatch = transporterConfig.useSilentFCMForForwardBatch
      let driverPickUpCharges = USRD.extractDriverPickupCharges farePolicy.farePolicyDetails
          parkingCharges = farePolicy.parkingCharge
      let safetyCharges = maybe 0 DCC.charge $ find (\ac -> DCC.SAFETY_PLUS_CHARGES == ac.chargeCategory) farePolicy.conditionalCharges
      return $ USRD.makeSearchRequestForDriverAPIEntity nearbyReq searchRequest searchTry bapMetadata popupDelaySeconds Nothing (Seconds 0) nearbyReq.vehicleServiceTier False isValueAddNP useSilentFCMForForwardBatch driverPickUpCharges parkingCharges safetyCharges (estimate >>= (.fareParams) >>= (.congestionCharge)) (estimate >>= (.fareParams) >>= (.petCharges)) (estimate >>= (.fareParams) >>= (.priorityCharges)) (estimate >>= (.fareParams) >>= (.tollCharges)) -- Seconds 0 as we don't know where he/she lies within the driver pool, anyways this API is not used in prod now.
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
  respondQuote (driverId, merchantId, merchantOpCityId) clientId Nothing Nothing Nothing Nothing Nothing DriverRespondReq {searchRequestId = Nothing, searchTryId = Just searchRequestId, notificationSource = Nothing, renderedAt = Nothing, respondedAt = Nothing, ..}

respondQuote :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Text -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> DriverRespondReq -> Flow APISuccess
respondQuote (driverId, merchantId, merchantOpCityId) clientId mbBundleVersion mbClientVersion mbConfigVersion mbReactBundleVersion mbDevice req = do
  searchTryId <- req.searchRequestId <|> req.searchTryId & fromMaybeM (InvalidRequest "searchTryId field is not present.")
  searchTry <- QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
  mSReqFD <- QSRD.findByDriverAndSearchTryId driverId searchTry.id
  sReqFD <-
    case mSReqFD of
      Just srfd -> return srfd
      Nothing -> do
        logWarning $ "Active Search request not found for the driver with driverId " <> driverId.getId <> " and searchTryId " <> searchTryId.getId
        if searchTry.status == DST.COMPLETED
          then throwError $ RideRequestAlreadyAccepted
          else throwError $ CustomerCancelled
  driverStats <- QDriverStats.findById driverId >>= fromMaybeM DriverInfoNotFound
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
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
              mbGeohash <- Redis.runInMultiCloudRedisMaybeResult $ Redis.withMasterRedis $ Redis.get (editDestinationUpdatedLocGeohashKey driverId)
              when (maybe False (sReqFD.previousDropGeoHash /=) mbGeohash) $ throwError CustomerDestinationUpdated
            let expiryTimeWithBuffer = addUTCTime 10 sReqFD.searchRequestValidTill ------ added 10 secs buffer so that if driver is accepting at last second then because of api latency it sholuldn't fail.
            when (expiryTimeWithBuffer < now) $ throwError (InvalidRequest "Quote can't be responded. SearchReqForDriver is expired")
            searchReq <- QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
            -- fetch if any booking exist with same transaction id and status in activeBookingStatus
            when (DTC.isDynamicOfferTrip searchTry.tripCategory) $ do
              mbActiveBooking <- runInMasterRedis $ QBE.findByTransactionIdAndStatuses searchReq.transactionId [DRB.NEW, DRB.TRIP_ASSIGNED]
              whenJust mbActiveBooking $ \_ ->
                throwError RideRequestAlreadyAccepted
            merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantDoesNotExist searchReq.providerId.getId)
            driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
            driverInfo <- QDriverInformation.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
            throwErrorOnRide transporterConfig.includeDriverCurrentlyOnRide driverInfo sReqFD.isForwardRequest
            when (sReqFD.response == Just Reject) $ do
              throwError QuoteAlreadyRejected
            whenM thereAreActiveQuotes (throwError FoundActiveQuotes)
            driverFCMPulledList <- case DTC.tripCategoryToPricingPolicy searchTry.tripCategory of
              DTC.EstimateBased _ -> acceptDynamicOfferDriverRequest merchant searchTry searchReq driver sReqFD mbBundleVersion mbClientVersion mbConfigVersion mbReactBundleVersion mbDevice reqOfferedValue driverStats transporterConfig
              DTC.QuoteBased _ -> acceptStaticOfferDriverRequest (Just searchTry) driver (fromMaybe searchTry.estimateId sReqFD.estimateId) reqOfferedValue merchant clientId transporterConfig
            when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ Analytics.updateOperatorAnalyticsAcceptationTotalRequestAndPassedCount driverId transporterConfig False True False False
            QSRD.updateDriverResponse (Just Accept) Inactive req.notificationSource req.renderedAt req.respondedAt sReqFD.id
            DS.driverScoreEventHandler merchantOpCityId $ buildDriverRespondEventPayload searchTry.id searchTry.requestId driverFCMPulledList
            unless (sReqFD.isForwardRequest) $ Redis.unlockRedis (editDestinationLockKey driverId)
          else do
            if not lockEditDestination
              then throwError $ DriverTransactionTryAgain Nothing
              else do
                void $ Redis.unlockRedis (editDestinationLockKey driverId)
    Reject -> do
      when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ Analytics.updateOperatorAnalyticsAcceptationTotalRequestAndPassedCount driverId transporterConfig False False True False
      QSRD.updateDriverResponse (Just Reject) Inactive req.notificationSource req.renderedAt req.respondedAt sReqFD.id
      DP.removeSearchReqIdFromMap merchantId driverId searchTry.requestId
      unlockRedisQuoteKeys
    Pulled -> do
      when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ Analytics.updateOperatorAnalyticsAcceptationTotalRequestAndPassedCount driverId transporterConfig False False False True
      QSRD.updateDriverResponse (Just Pulled) Inactive req.notificationSource req.renderedAt req.respondedAt sReqFD.id
      throwError UnexpectedResponseValue
  pure Success
  where
    buildDriverRespondEventPayload searchTryId searchReqId restActiveDriverSearchReqs =
      DST.OnDriverAcceptingSearchRequest
        { restDriverIds = map (.driverId) restActiveDriverSearchReqs,
          response = req.response,
          ..
        }
    unlockRedisQuoteKeys = do
      Redis.unlockRedis (offerQuoteLockKeyWithCoolDown driverId)
      Redis.unlockRedis (editDestinationLockKey driverId)
    callWithErrorHandling func = do
      exep <- withTryCatch "callWithErrorHandling:respondQuote" func
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
      (MonadFlow m, CoreMetrics m, CacheFlow m r, EsqDBFlow m r, MonadReader r m, HasField "driverQuoteExpirationSeconds" r NominalDiffTime, HasField "version" r DeploymentVersion) =>
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
      Maybe Text ->
      m DDrQuote.DriverQuote
    buildDriverQuote driver driverStats searchReq sd estimateId tripCategory fareParams mbBundleVersion' mbClientVersion' mbConfigVersion' mbReactBundleVersion' mbDevice' = do
      guid <- generateGUID
      now <- getCurrentTime
      deploymentVersion <- asks (.version)
      transporterConfig <- CTC.findByMerchantOpCityId searchReq.merchantOperatingCityId (Just (TransactionId (Id searchReq.transactionId))) >>= fromMaybeM (TransporterConfigNotFound searchReq.merchantOperatingCityId.getId)
      if tripCategory == DTC.OneWay DTC.OneWayOnDemandDynamicOffer && transporterConfig.isDynamicPricingQARCalEnabled == Just True
        then do
          void $ Redis.withCrossAppRedis $ Redis.geoAdd (mkAcceptanceVehicleCategoryWithDistanceBin now sd.vehicleCategory ((.getMeters) <$> searchReq.estimatedDistance)) [(searchReq.fromLocation.lon, searchReq.fromLocation.lat, TE.encodeUtf8 (sd.searchTryId.getId))]
          void $ Redis.withCrossAppRedis $ Redis.expire (mkAcceptanceVehicleCategoryWithDistanceBin now sd.vehicleCategory ((.getMeters) <$> searchReq.estimatedDistance)) 3600
          void $ Redis.withCrossAppRedis $ Redis.geoAdd (mkAcceptanceVehicleCategory now sd.vehicleCategory) [(searchReq.fromLocation.lon, searchReq.fromLocation.lat, TE.encodeUtf8 (sd.searchTryId.getId))]
          void $ Redis.withCrossAppRedis $ Redis.expire (mkAcceptanceVehicleCategory now sd.vehicleCategory) 3600
          void $ Redis.withCrossAppRedis $ Redis.incr (mkAcceptanceVehicleCategoryCity now sd.vehicleCategory searchReq.merchantOperatingCityId.getId)
          void $ Redis.withCrossAppRedis $ Redis.expire (mkAcceptanceVehicleCategoryCity now sd.vehicleCategory searchReq.merchantOperatingCityId.getId) 3600
        else pure ()
      driverQuoteExpirationSeconds <- asks (.driverQuoteExpirationSeconds)
      let estimatedFare = fareSum fareParams $ Just sd.conditionalCharges
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
            specialLocationName = searchReq.specialLocationName,
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
            vehicleServiceTierName = sd.vehicleServiceTierName,
            coinsRewardedOnGoldTierRide = sd.coinsRewardedOnGoldTierRide,
            reactBundleVersion = driver.reactBundleVersion <|> mbReactBundleVersion',
            commissionCharges = sd.commissionCharges
          }
    thereAreActiveQuotes = do
      driverUnlockDelay <- asks (.driverUnlockDelay)
      activeQuotes <- QDrQt.findActiveQuotesByDriverId driverId driverUnlockDelay
      logDebug $ "active quotes for driverId = " <> driverId.getId <> show activeQuotes
      pure $ not $ null activeQuotes
    getQuoteLimit dist vehicleServiceTier tripCategory searchReq area searchRepeatType searchRepeatCounter = do
      driverPoolCfg <- SCDPC.getDriverPoolConfig merchantOpCityId vehicleServiceTier tripCategory area dist searchRepeatType searchRepeatCounter (Just (TransactionId (Id searchReq.transactionId))) searchReq
      pure driverPoolCfg.driverQuoteLimit

    acceptDynamicOfferDriverRequest :: DM.Merchant -> DST.SearchTry -> DSR.SearchRequest -> SP.Person -> SearchRequestForDriver -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> Maybe HighPrecMoney -> DStats.DriverStats -> TransporterConfig -> Flow [SearchRequestForDriver]
    acceptDynamicOfferDriverRequest merchant searchTry searchReq driver sReqFD mbBundleVersion' mbClientVersion' mbConfigVersion' mbReactBundleVersion' mbDevice' reqOfferedValue driverStats transporterConfig = do
      let estimateId = fromMaybe searchTry.estimateId sReqFD.estimateId -- backward compatibility
      logDebug $ "offered fare: " <> show reqOfferedValue
      quoteLimit <- getQuoteLimit searchReq.estimatedDistance sReqFD.vehicleServiceTier searchTry.tripCategory searchReq (fromMaybe SL.Default searchReq.area) searchTry.searchRepeatType searchTry.searchRepeatCounter
      quoteCount <- runInReplica $ QDrQt.countAllBySTId searchTry.id
      when (quoteCount >= quoteLimit) (throwError QuoteAlreadyRejected)
      farePolicy <- getFarePolicyByEstOrQuoteId (Just $ Maps.getCoordinates searchReq.fromLocation) (Just . Maps.getCoordinates =<< searchReq.toLocation) searchReq.fromLocGeohash searchReq.toLocGeohash searchReq.estimatedDistance searchReq.estimatedDuration merchantOpCityId searchTry.tripCategory sReqFD.vehicleServiceTier searchReq.area estimateId Nothing Nothing searchReq.dynamicPricingLogicVersion (Just (TransactionId (Id searchReq.transactionId))) searchReq.configInExperimentVersions searchReq.specialLocationName
      let driverExtraFeeBounds = DFarePolicy.findDriverExtraFeeBoundsByDistance (fromMaybe 0 searchReq.estimatedDistance) <$> farePolicy.driverExtraFeeBounds
      whenJust reqOfferedValue $ \off ->
        whenJust driverExtraFeeBounds $ \driverExtraFeeBounds' ->
          unless (isAllowedExtraFee driverExtraFeeBounds' off) $
            throwError $ NotAllowedExtraFee $ show off
      unlessM (validateSearchTryActive searchTry.id) $ do
        logError ("RideRequestAlreadyAcceptedOrCancelled " <> "in respond quote for searchTryId:" <> getId searchTry.id <> " estimateId:" <> estimateId <> " driverId:" <> getId driver.id <> " and srfdId:" <> getId sReqFD.id)
        throwError (RideRequestAlreadyAcceptedOrCancelled sReqFD.id.getId)
      fareParams <- do
        FCV2.calculateFareParametersV2
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
              driverSelectedFare = reqOfferedValue,
              customerExtraFee = searchTry.customerExtraFee,
              petCharges = if isJust searchTry.petCharges then farePolicy.petCharges else Nothing,
              nightShiftCharge = Nothing,
              customerCancellationDues = searchReq.customerCancellationDues,
              tollCharges = searchReq.tollCharges,
              estimatedRideDuration = searchReq.estimatedDuration,
              nightShiftOverlapChecking = DTC.isFixedNightCharge searchTry.tripCategory,
              estimatedCongestionCharge = Nothing,
              estimatedDistance = searchReq.estimatedDistance,
              timeDiffFromUtc = Nothing,
              currency = searchReq.currency,
              shouldApplyBusinessDiscount = searchTry.billingCategory == SLT.BUSINESS,
              shouldApplyPersonalDiscount = searchTry.billingCategory == SLT.PERSONAL,
              distanceUnit = searchReq.distanceUnit,
              merchantOperatingCityId = Just merchantOpCityId,
              mbAdditonalChargeCategories = Just sReqFD.conditionalCharges,
              numberOfLuggages = searchReq.numberOfLuggages,
              ..
            }
      driverQuote <- buildDriverQuote driver driverStats searchReq sReqFD estimateId searchTry.tripCategory fareParams mbBundleVersion' mbClientVersion' mbConfigVersion' mbReactBundleVersion' mbDevice'
      void $ cacheFarePolicyByQuoteId driverQuote.id.getId farePolicy
      triggerQuoteEvent QuoteEventData {quote = driverQuote}
      void $ QDrQt.create driverQuote
      driverFCMPulledList <-
        if (quoteCount + 1) >= quoteLimit || (searchReq.autoAssignEnabled == Just True)
          then runInMasterRedis $ QSRD.findAllActiveBySTId searchTry.id DSRD.Active
          else pure []
      pullExistingRideRequests merchantOpCityId driverFCMPulledList merchantId driver.id (mkPrice (Just driverQuote.currency) driverQuote.estimatedFare) transporterConfig
      sendDriverOffer merchant searchReq sReqFD searchTry driverQuote
      return driverFCMPulledList
      where
        validateSearchTryActive searchTryId = do
          -- Lock Description: This is a Lock held between Driver Respond and Cancel Search, if UI Cancel Search is OnGoing then the SearchTry will be marked as CANCELLED and Driver Respond will fail with `RideRequestAlreadyAcceptedOrCancelled`.
          -- Lock Release: Held for 5 seconds once acquired, never released.
          isLockAcquired <- CS.lockSearchTry searchTryId
          if isLockAcquired
            then do
              mbUpdatedSearchTry <- runInMasterDbAndRedis $ QST.findById searchTryId
              return $ maybe True (\updatedSearchTry -> updatedSearchTry.status == DST.ACTIVE) mbUpdatedSearchTry
            else do
              return False

acceptStaticOfferDriverRequest :: Maybe DST.SearchTry -> SP.Person -> Text -> Maybe HighPrecMoney -> DM.Merchant -> Maybe Text -> TransporterConfig -> Flow [SearchRequestForDriver]
acceptStaticOfferDriverRequest mbSearchTry driver quoteId reqOfferedValue merchant clientId transporterConfig = do
  whenJust reqOfferedValue $ \_ -> throwError (InvalidRequest "Driver can't offer fare in static trips")
  quote <- QQuote.findById (Id quoteId) >>= fromMaybeM (QuoteNotFound quoteId)
  booking <- QBooking.findByQuoteId quote.id.getId >>= fromMaybeM (BookingDoesNotExist quote.id.getId)
  when booking.isScheduled $ removeBookingFromRedis booking
  isBookingAssignmentInprogress' <- CS.isBookingAssignmentInprogress booking.id
  when isBookingAssignmentInprogress' $ throwError RideRequestAlreadyAccepted
  isBookingCancelled' <- CS.isBookingCancelled booking.id
  when isBookingCancelled' $ throwError (InternalError "BOOKING_CANCELLED")
  CS.markBookingAssignmentInprogress booking.id -- this is to handle booking assignment and user cancellation at same time
  unless (booking.status == DRB.NEW) $ throwError RideRequestAlreadyAccepted
  mFleetAssociation <- QFDA.findByDriverId driver.id True
  whenJust mbSearchTry $ \searchTry -> do
    QST.updateStatus DST.COMPLETED searchTry.id
    mSReqFD <- QSRD.findByDriverAndSearchTryId driver.id searchTry.id
    whenJust mSReqFD $ \sReqFD -> QBooking.updateDqDurationToPickup booking.id sReqFD.durationToPickup
  (ride, _, vehicle) <- initializeRide merchant driver booking Nothing Nothing clientId Nothing (mFleetAssociation <&> (.fleetOwnerId) <&> Id)
  driverFCMPulledList <-
    case mbSearchTry of
      Just searchTry -> deactivateExistingQuotes booking.merchantOperatingCityId merchant.id driver.id searchTry.id (mkPrice (Just quote.currency) quote.estimatedFare) (Just transporterConfig)
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

getEarnings :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Day -> Day -> DCommon.EarningType -> Flow DCommon.EarningPeriodStatsRes
getEarnings (driverId, _, merchantOpCityId) from to earningType = do
  when (from > to) $
    throwError $ InvalidRequest $ "Start date must not be after end date."
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let earningsWindowSize = transporterConfig.analyticsConfig.earningsWindowSize
  case earningType of
    DCommon.DAILY -> do
      when (daysBetween > earningsWindowSize) $
        throwError $ InvalidRequest $ "For daily earnings, the date range must be less than or equal to " <> T.pack (show earningsWindowSize) <> " days (inclusive)."
      driverStats <- runInReplica $ SQDS.findAllInRangeByDriverId_ driverId from to
      let dailyEarningData = CHDS.mkEarningsBar <$> map (\d -> (driverId, d.merchantLocalDate, d.totalEarnings, d.totalDistance, d.numRides, d.cancellationCharges, d.bonusEarnings)) driverStats
      pure $ mkEarningPeriodStatsRes dailyEarningData
    DCommon.WEEKLY -> do
      when (weeksBetween > earningsWindowSize) $
        throwError $ InvalidRequest $ "For weekly earnings, the date range must be less than or equal to " <> T.pack (show earningsWindowSize) <> " weeks (inclusive)."
      let weekStartMode = transporterConfig.analyticsConfig.weekStartMode
      weeklyEarningData <- runInReplica $ CHDS.aggregatePeriodStatsWithBoundaries driverId from to (CHDS.WeeklyStats weekStartMode)
      pure $ mkEarningPeriodStatsRes weeklyEarningData
    DCommon.MONTHLY -> do
      when (monthsBetween > earningsWindowSize) $
        throwError $ InvalidRequest $ "For monthly earnings, the date range must be less than or equal to " <> T.pack (show earningsWindowSize) <> " months (inclusive)."
      monthlyEarningData <- runInReplica $ CHDS.aggregatePeriodStatsWithBoundaries driverId from to CHDS.MonthlyStats
      pure $ mkEarningPeriodStatsRes monthlyEarningData
  where
    mkEarningPeriodStatsRes :: [CHDS.EarningsBar] -> DCommon.EarningPeriodStatsRes
    mkEarningPeriodStatsRes earningsBar =
      let earningData = map (\e -> DCommon.EarningPeriodStats {periodStart = e.periodStartDate, totalEarnings = roundToIntegral e.earnings, totalDistance = e.distance, totalRides = e.rides, cancellationCharges = roundToIntegral e.cancellationChargesReceived, tipAmount = roundToIntegral e.bonusEarningsReceived}) earningsBar
       in DCommon.EarningPeriodStatsRes {earnings = earningData}

    daysBetween :: Int
    daysBetween =
      let days_diff = diffDays to from
       in fromIntegral days_diff + 1

    weeksBetween :: Int
    weeksBetween =
      let (fromYear, fromWeek, _) = toWeekDate from
          (toYear, toWeek, _) = toWeekDate to
          fullYearRange = [fromYear .. toYear -1]
          weeksInFullYears = sum $ map totalWeeksOfYear fullYearRange
          weeksInPartialYear = toWeek - fromWeek + 1
       in weeksInFullYears + weeksInPartialYear

    monthsBetween :: Int
    monthsBetween =
      let (fromYear, fromMonth, _) = toGregorian from
          (toYear, toMonth, _) = toGregorian to
       in fromIntegral (toYear - fromYear) * 12 + (toMonth - fromMonth + 1)

    totalWeeksOfYear :: Integer -> Int
    totalWeeksOfYear year =
      let lastDayOfYear = fromGregorian year 12 31
          (yearData, lastWeek, _) = toWeekDate lastDayOfYear
       in if yearData == year then lastWeek else 52

driverPhotoUploadHitsCountKey :: Id SP.Person -> Text
driverPhotoUploadHitsCountKey driverId = "BPP:ProfilePhoto:verify:" <> getId driverId <> ":hitsCount"

driverQrUploadHitsCountKey :: Id SP.Person -> Text
driverQrUploadHitsCountKey driverId = "BPP:QRImage" <> getId driverId <> ":hitsCount"

driverVehiclePhotoUploadHitsCountKey :: Id SP.Person -> Text
driverVehiclePhotoUploadHitsCountKey driverId = "BPP:VehiclePhoto:verify:" <> getId driverId <> ":hitsCount"

driverProfileImagesUpload :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Issue.IssueMediaUploadReq -> Flow Issue.IssueMediaUploadRes
driverProfileImagesUpload (driverId, merchantId, merchantOpCityId) Issue.IssueMediaUploadReq {..} = do
  Issue.issueMediaUpload' (cast driverId, cast merchantId, cast merchantOpCityId) driverIssueHandle Issue.IssueMediaUploadReq {..} "driver-profile-images" ("driverId-" <> getId driverId)

driverPhotoUpload :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> DriverPhotoUploadReq -> Flow APISuccess
driverPhotoUpload (driverId, merchantId, merchantOpCityId) DriverPhotoUploadReq {..} = do
  let imageType_ = fromMaybe ProfilePhoto imageType
  let (hitsCountKey, basePath, domain) = case imageType_ of
        ProfilePhoto -> (driverPhotoUploadHitsCountKey, "/driver-profile-picture/", "driver/profile/photo")
        QrImage -> (driverQrUploadHitsCountKey, "/driver-qr/", "driver/qr/image")
        VehiclePhoto -> (driverVehiclePhotoUploadHitsCountKey, "/driver-vehicle-photo/", "driver/vehicle/photo")
  checkSlidingWindowLimit (hitsCountKey driverId)
  person <- runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonNotFound (getId driverId))
  imageExtension <- validateContentType
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast driverId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  mbRc <-
    case rcNo of
      Just rcNumber -> runInReplica $ QRC.findLastVehicleRCWrapper rcNumber
      Nothing -> pure Nothing

  when (imageType_ == ProfilePhoto && transporterConfig.enableFaceVerification) $ do
    let req = IF.FaceValidationReq {file = image, brisqueFeatures}
    void $ validateFaceImage merchantId merchantOpCityId req

  filePath <- S3.createFilePath basePath ("driver-" <> getId driverId) fileType imageExtension
  let fileUrl =
        transporterConfig.mediaFileUrlPattern
          & T.replace "<DOMAIN>" domain
          & T.replace "<FILE_PATH>" filePath

  result <- withTryCatch "S3:put:driverPhotoUpload" $ S3.put (T.unpack filePath) image
  case result of
    Left err -> throwError $ InternalError ("S3 Upload Failed: " <> show err)
    Right _ -> do
      case imageType_ of
        ProfilePhoto -> whenJust person.faceImageId $ \mediaFileId -> do
          QPerson.updateMediaId driverId Nothing
          MFQuery.deleteById mediaFileId
        QrImage -> whenJust person.qrImageId $ \mediaFileId -> do
          QPerson.updateQrMediaId driverId Nothing
          MFQuery.deleteById mediaFileId
        VehiclePhoto -> do
          whenJust mbRc $ \rc -> do
            whenJust rc.vehicleImageId $ \mediaFileId -> do
              QRC.updateVehicleImageId Nothing rc.id
              QVehicle.updateVehicleImageId Nothing driverId
              MFQuery.deleteById mediaFileId
  createMediaEntry driverId Common.AddLinkAsMedia {url = fileUrl, fileType} filePath imageType_ mbRc
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

createMediaEntry :: Id SP.Person -> Common.AddLinkAsMedia -> Text -> ImageType -> Maybe VehicleRegistrationCertificate -> Flow APISuccess
createMediaEntry driverId Common.AddLinkAsMedia {..} filePath imageType mbRc = do
  fileEntity <- mkFile url
  MFQuery.create fileEntity
  case imageType of
    ProfilePhoto -> QPerson.updateMediaId driverId (Just fileEntity.id)
    QrImage -> QPerson.updateQrMediaId driverId (Just fileEntity.id)
    VehiclePhoto -> do
      whenJust mbRc $ \rc -> do
        QRC.updateVehicleImageId (Just fileEntity.id) rc.id
        QVehicle.updateVehicleImageId (Just fileEntity.id) driverId
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
    HasFlowEnv m r ["apiRateLimitOptions" ::: APIRateLimitOptions, "smsCfg" ::: SmsConfig],
    HasKafkaProducer r
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
      (mbSender, message, templateId) <-
        MessageBuilder.buildSendAlternateNumberOTPMessage merchantOpCityId $
          MessageBuilder.BuildSendOTPMessageReq
            { otp = otpCode,
              hash = otpHash
            }
      let sender = fromMaybe smsCfg.sender mbSender
      Sms.sendSMS person.merchantId merchantOpCityId (Sms.SendSMSReq message altPhoneNumber sender templateId)
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
    verified <- Redis.withMasterRedis (Redis.get (makeAlternateNumberVerifiedKey personId)) >>= fromMaybeM (InvalidRequest "Verified not found")
    when verified $ throwError $ AuthBlocked "Already verified."
    altMobNo <- Redis.withMasterRedis (Redis.get (makeAlternatePhoneNumberKey personId)) >>= fromMaybeM (InvalidRequest "Alternate Number not found")
    val <- Redis.withMasterRedis $ Redis.get (makeAlternateNumberOtpKey personId)
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
    CacheFlow m r,
    HasKafkaProducer r
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
    (mbSender, message, templateId) <-
      MessageBuilder.buildSendAlternateNumberOTPMessage merchantOpCityId $
        MessageBuilder.BuildSendOTPMessageReq
          { otp = otpCode,
            hash = otpHash
          }
    let sender = fromMaybe smsCfg.sender mbSender
    Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message altphoneNumber sender templateId)
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
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r, EncFlow m r, HasField "smsCfg" r SmsConfig, MonadFlow m, HasKafkaProducer r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ServiceNames ->
  Maybe ClearManualSelectedDues ->
  Maybe SPayment.DeepLinkData ->
  m ClearDuesRes
clearDriverDues (personId, _merchantId, opCityId) serviceName clearSelectedReq mbDeepLinkData = do
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCityId Nothing serviceName
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
  successfulInvoices <- mapM (\fee -> runInReplica (QINV.findInvoiceByFeeIdAndStatus fee.id Domain.SUCCESS)) dueDriverFees'
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
      [] -> mkClearDuesResp <$> SPayment.createOrder (personId, _merchantId, opCityId) paymentService (dueDriverFees, []) Nothing INV.MANUAL_INVOICE Nothing vendorFees mbDeepLinkData splitEnabled Nothing
      (invoice_ : restinvoices) -> do
        mapM_ (QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE . (.id)) restinvoices
        (invoice, currentDuesForExistingInvoice, newDues) <- validateExistingInvoice invoice_ dueDriverFees
        let driverFeeForCurrentInvoice = filter (\dfee -> dfee.id.getId `elem` currentDuesForExistingInvoice) dueDriverFees
        let driverFeeToBeAddedOnExpiry = filter (\dfee -> dfee.id.getId `elem` newDues) dueDriverFees
        mkClearDuesResp <$> SPayment.createOrder (personId, _merchantId, opCityId) paymentService (driverFeeForCurrentInvoice, driverFeeToBeAddedOnExpiry) Nothing INV.MANUAL_INVOICE invoice vendorFees mbDeepLinkData splitEnabled Nothing
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
    refundedAmount :: Maybe HighPrecMoney,
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
      nearestAndSourceCity <- withTryCatch "getNearestOperatingAndSourceCity:getCity" $ getNearestOperatingAndSourceCity merchant latLng
      case nearestAndSourceCity of
        Left _ -> return GetCityResp {city = Nothing, status = APISuccess.Success}
        Right nearestSourceCity -> return GetCityResp {city = Just $ show nearestSourceCity.nearestOperatingCity.city, status = APISuccess.Success}
    Nothing -> do
      geometry <- runInReplica $ QGeometry.findGeometriesContainingGps latLng
      case filter (\geom -> geom.city /= Context.City "AnyCity") geometry of
        [] ->
          find (\geom -> geom.city == Context.City "AnyCity") geometry & \case
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
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasKafkaProducer r
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
              driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
              vehicle <- runInReplica $ QVehicle.findById personId >>= fromMaybeM (VehicleDoesNotExist personId.getId)
              cityServiceTiers <- CQVST.findAllByMerchantOpCityId cityId Nothing
              let availableServiceTierItems = map fst $ filter (not . snd) (selectVehicleTierForDriverWithUsageRestriction False driverInfo vehicle cityServiceTiers)
              let availableServiceTiers = (.serviceTierType) <$> availableServiceTierItems
              let mbScheduleBookingListEligibilityTags = listToMaybe availableServiceTierItems >>= (.scheduleBookingListEligibilityTags)
              let scheduleEnabled = maybe True (not . null . intersect (maybe [] ((LYT.getTagNameValue . Yudhishthira.removeTagExpiry) <$>) driver.driverTag)) mbScheduleBookingListEligibilityTags
              if scheduleEnabled
                then do
                  let vehicleVariants = nub $ castServiceTierToVariant <$> availableServiceTiers
                      tripCategory = maybe possibleScheduledTripCategories (: []) mbTripCategory
                      limit = fromMaybe 10 mbLimit
                      offset = fromMaybe 0 mbOffset
                      safelimit = toInteger transporterConfig.recentScheduledBookingsSafeLimit
                  scheduledBookings <- getScheduledBookings from cityId vehicleVariants (Just dLoc) transporterConfig (Just availableServiceTiers) tripCategory limit offset safelimit
                  bookings <- mapM (buildBookingAPIEntityFromBooking mbDLoc) (catMaybes scheduledBookings)
                  let sortedBookings = sortBookingsByDistance (catMaybes bookings)
                  return $ ScheduledBookingRes sortedBookings
                else return $ ScheduledBookingRes []
        _ -> pure $ ScheduledBookingRes []
  where
    getCurrentDriverLocUsingLTS driverId = do
      result <- withTryCatch "driversLocation:getCurrentDriverLocUsingLTS" $ LTF.driversLocation [driverId]
      return $ case result of
        Left _ -> Nothing
        Right locations -> listToMaybe locations >>= \x -> Just LatLong {lat = x.lat, lon = x.lon}

    possibleScheduledTripCategories :: [DTC.TripCategory]
    possibleScheduledTripCategories = [DTC.Rental DTC.OnDemandStaticOffer, DTC.InterCity DTC.OneWayOnDemandStaticOffer Nothing, DTC.OneWay DTC.OneWayOnDemandStaticOffer]

    sortBookingsByDistance :: [ScheduleBooking] -> [ScheduleBooking]
    sortBookingsByDistance = sortBy (compareDistances `on` (\booking -> booking.bookingDetails.distanceToPickup))

    compareDistances :: Maybe Meters -> Maybe Meters -> Ordering
    compareDistances (Just d1) (Just d2) = compare (getMeters d1) (getMeters d2)
    compareDistances (Just _) Nothing = LT
    compareDistances Nothing (Just _) = GT
    compareDistances Nothing Nothing = EQ

filterNearbyBookings :: UTCTime -> LatLong -> [(Text, Double, Double, UTCTime, ServiceTierType)] -> Maybe [ServiceTierType] -> [Text]
filterNearbyBookings currentTime dLoc parsedRes mbPossibleServiceTierTypes =
  map (\(id, _, _, _, _) -> id) $
    filter
      ( \(_, lat, lon, pickupTime, bookingServiceTier) ->
          checkNearbyBookingsWithServiceTier currentTime pickupTime lat lon dLoc mbPossibleServiceTierTypes bookingServiceTier
      )
      parsedRes

checkNearbyBookingsWithServiceTier :: UTCTime -> UTCTime -> Double -> Double -> LatLong -> Maybe [ServiceTierType] -> ServiceTierType -> Bool
checkNearbyBookingsWithServiceTier currentTime pickupTime lat lon dLoc mbPossibleServiceTierTypes bookingServiceTierType =
  let bookingLoc = LatLong {lat = lat, lon = lon}
      distanceToPickup = highPrecMetersToMeters $ distanceBetweenInMeters bookingLoc dLoc
      distanceInKm = metersToKilometers distanceToPickup
      avgSpeedOfVehicleInKM :: Int = 25 -- consider avg speed of vehicle to be 25 kmph
      speedInMinPerKm = if avgSpeedOfVehicleInKM == 0 then 3 else truncate (60.0 / toRational avgSpeedOfVehicleInKM)
      estimatedTime = intToNominalDiffTime $ distanceInKm.getKilometers * speedInMinPerKm * 60
      isValidServiceTierType = maybe True (bookingServiceTierType `elem`) mbPossibleServiceTierTypes
   in (isValidServiceTierType && addUTCTime estimatedTime currentTime <= pickupTime)

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

-- Filters booking IDs by distance/time and fetches full booking data.
-- Parses member strings to extract location/time, checks if driver can reach in time.
-- Returns full booking objects from hash set for bookings within reach.
returnFilteredBookings :: UTCTime -> [BS.ByteString] -> Maybe LatLong -> Maybe [ServiceTierType] -> Text -> Integer -> Flow [Maybe DRB.Booking]
returnFilteredBookings now res mbDLoc mbPossibleServiceTierTypes redisKeyForHset limit = do
  let parsedRes = mapMaybe (parseMember . decodeUtf8) res
      nearbyBookings = case mbDLoc of
        Just dLoc -> take (fromIntegral limit) $ filterNearbyBookings now dLoc parsedRes mbPossibleServiceTierTypes
        Nothing -> take (fromIntegral limit) $ map (\(id, _, _, _, _) -> id) parsedRes
  if not $ null nearbyBookings
    then Redis.hmGet redisKeyForHset nearbyBookings
    else pure []

buildBookingAPIEntityFromBooking :: Maybe LatLong -> DRB.Booking -> Flow (Maybe ScheduleBooking)
buildBookingAPIEntityFromBooking mbDriverLocation DRB.Booking {..} = do
  let pickup = LatLong {lat = fromLocation.lat, lon = fromLocation.lon}
      distanceToPickup' = highPrecMetersToMeters . (`distanceBetweenInMeters` pickup) <$> mbDriverLocation
  mbQuote <- QQuote.findById (Id quoteId)
  case mbQuote of
    Nothing -> do
      fork "Error in case of no quote - Potential drainer lag" $ throwError (ShouldNotHappen $ "Quote with quoteId = \"" <> quoteId <> "\" not found.")
      pure Nothing
    Just quote -> do
      let farePolicyBreakups = maybe [] (mkFarePolicyBreakups Prelude.id (mkBreakupItem currency) estimatedDistance quote.fareParams.customerCancellationDues Nothing estimatedFare quote.fareParams.congestionChargeViaDp) quote.farePolicy
      return $ Just $ ScheduleBooking BookingAPIEntity {distanceToPickup = distanceToPickup', isInsured = Just isInsured, ..} (catMaybes farePolicyBreakups)

mkBreakupItem :: Currency -> Text -> Text -> Maybe DOVT.RateCardItem
mkBreakupItem currency title valueInText = do
  priceObject <- DOV.stringToPrice currency valueInText
  return $
    DOVT.RateCardItem
      { title,
        price = priceObject.amountInt,
        priceWithCurrency = mkPriceAPIEntity priceObject
      }

-- Retrieves scheduled bookings for a specified day from Redis.
-- Gets current time and local time internally, then determines query strategy based on day comparison.
-- For today: queries two windows (30min-2hrs with safelimit, 2hrs-midnight with limit), skips next 30min.
-- For future days: queries full 24-hour window from midnight to midnight with limit.
-- Uses local time for Redis keys/scores, converts to UTC for distance filtering.
getScheduledBookings :: Day -> Id DMOC.MerchantOperatingCity -> [VehicleVariant] -> Maybe LatLong -> TransporterConfig -> Maybe [ServiceTierType] -> [DTC.TripCategory] -> Integer -> Integer -> Integer -> Flow [Maybe DRB.Booking]
getScheduledBookings from mocCityId vehicleVariants mbDLoc transporterConfig mbPossibleServiceTierTypes tripCategories limit offset safelimit = do
  now <- getCurrentTime
  let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
      localNow = addUTCTime timeDiff now
      isToday = utctDay now >= from
      referenceTime = if isToday then localNow else addUTCTime timeDiff (UTCTime from 0)
      redisKeys = createRedisKeysForCombinations referenceTime mocCityId tripCategories vehicleVariants
      redisKeyForHset = createRedisKeyForHset referenceTime mocCityId
      queryTimeRange startTime endTime off lim =
        concat <$> mapM (\key -> Redis.zRangeByScoreByCount key (calculateSortedSetScore startTime) (calculateSortedSetScore endTime) off lim) redisKeys
      fetchTodayBookings = do
        let window1Start = addUTCTime 1800 localNow
            window1End = addUTCTime 7200 localNow
            window2Start = addUTCTime 7201 localNow
            window2End = UTCTime (addDays 1 (utctDay localNow)) 0
        res1 <- queryTimeRange window1Start window1End offset safelimit
        let remainingOffset = max 0 (offset - toInteger (length res1))
        res2 <- queryTimeRange window2Start window2End remainingOffset limit
        return $ res1 ++ res2
      fetchFutureBookings =
        queryTimeRange referenceTime (addUTCTime 86400 referenceTime) offset limit
  res <- if isToday then fetchTodayBookings else fetchFutureBookings
  returnFilteredBookings now res mbDLoc mbPossibleServiceTierTypes redisKeyForHset limit

acceptScheduledBooking ::
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Text ->
  Id DRB.Booking ->
  Flow APISuccess
acceptScheduledBooking (personId, merchantId, merchantOpCityId) clientId bookingId = do
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigDoesNotExist merchantOpCityId.getId)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  booking <- runInReplica $ QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  driver <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  upcomingOrActiveRide <- runInReplica $ QRide.getUpcomingOrActiveByDriverId driver.id
  unless (isNothing upcomingOrActiveRide) $ throwError (RideInvalidStatus "Cannot accept booking during active or already having upcoming ride.")
  mbActiveSearchTry <- QST.findActiveTryByQuoteId booking.quoteId
  void $ acceptStaticOfferDriverRequest mbActiveSearchTry driver booking.quoteId Nothing merchant clientId transporterConfig
  pure Success

clearDriverFeeWithCreate ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r, HasField "smsCfg" r SmsConfig, HasKafkaProducer r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  ServiceNames ->
  (HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney) ->
  DDF.FeeType ->
  Currency ->
  Maybe SPayment.DeepLinkData ->
  Bool ->
  m ClearDuesRes
clearDriverFeeWithCreate (personId, merchantId, opCityId) serviceName (fee', mbCgst, mbSgst) feeType currency mbDeepLinkData sendPaymentLink = do
  dueDriverFee <- QDF.findAllFeeByTypeServiceStatusAndDriver serviceName personId [feeType] [DDF.PAYMENT_PENDING]
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCityId Nothing serviceName
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
  -- Mark all previous pending fees as INACTIVE before creating new entry
  now <- getCurrentTime
  unless (null dueDriverFee) $ do
    let oldFeeIds = map (.id) dueDriverFee
    -- Find and mark only ACTIVE_INVOICE invoices as INACTIVE to avoid touching paid/failed invoices
    oldInvoices <- mapM (\oldFee -> QINV.findActiveManualInvoiceByFeeId oldFee.id (feeTypeToInvoicetype feeType) Domain.ACTIVE_INVOICE) dueDriverFee
    mapM_ (mapM_ (QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE . (.id))) oldInvoices
    QDF.updateStatusByIds DDF.INACTIVE oldFeeIds now
  -- Always create a new driver fee entry
  driverFee' <- mkDriverFee fee cgst sgst vehicleCategory
  QDF.create driverFee'
  let driverFee = [driverFee']
  invoices <- mapM (\fee_ -> runInReplica (QINV.findActiveManualInvoiceByFeeId fee_.id (feeTypeToInvoicetype feeType) Domain.ACTIVE_INVOICE)) driverFee
  let paymentService = subscriptionConfig.paymentServiceName
      sortedInvoices = mergeSortAndRemoveDuplicate invoices
      splitEnabled = subscriptionConfig.isVendorSplitEnabled == Just True
  vendorFees' <- if splitEnabled then concat <$> mapM (QVF.findAllByDriverFeeId . DDF.id) driverFee else pure []
  let vendorFees = map SPayment.roundVendorFee vendorFees'
  resp <- do
    case sortedInvoices of
      -- if no invoice is present, then create a new invoice for all the driver fees
      [] -> do mkClearDuesResp <$> SPayment.createOrder (personId, merchantId, opCityId) paymentService (driverFee, []) Nothing (feeTypeToInvoicetype feeType) Nothing vendorFees mbDeepLinkData splitEnabled Nothing
      (invoice_ : restinvoices) -> do
        mapM_ (QINV.updateInvoiceStatusByInvoiceId INV.INACTIVE . (.id)) restinvoices
        (invoice, currentDuesForExistingInvoice, newDues) <- validateExistingInvoice invoice_ driverFee
        let driverFeeForCurrentInvoice = filter (\dfee -> dfee.id.getId `elem` currentDuesForExistingInvoice) driverFee
        let driverFeeToBeAddedOnExpiry = filter (\dfee -> dfee.id.getId `elem` newDues) driverFee
        mkClearDuesResp <$> SPayment.createOrder (personId, merchantId, opCityId) paymentService (driverFeeForCurrentInvoice, driverFeeToBeAddedOnExpiry) Nothing (feeTypeToInvoicetype feeType) invoice vendorFees mbDeepLinkData splitEnabled Nothing
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
            validDays = Nothing,
            cancellationPenaltyAmount = Nothing,
            addedToFeeId = Nothing,
            collectedAtVendorId = Nothing,
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

verifyVpaStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r, HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl], HasKafkaProducer r) => (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> m APISuccess
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
  driverFees <- runInReplica $ QDF.findAllFeeByTypeServiceStatusAndDriver serviceName personId [DDF.ONE_TIME_SECURITY_DEPOSIT] [DDF.PAYMENT_PENDING, DDF.CLEARED, DDF.PAYMENT_OVERDUE, DDF.EXEMPTED, DDF.COLLECTED_CASH, DDF.REFUND_FAILED, DDF.REFUNDED]
  concat <$> mapM buildSecurityDepositDfStatus (sortOn (.createdAt) driverFees)
  where
    buildSecurityDepositDfStatus dfee = do
      let securityDepositAmount = SLDriverFee.roundToHalf dfee.currency dfee.govtCharges + dfee.platformFee.fee + dfee.platformFee.cgst + dfee.platformFee.sgst
          securityDepositAmountWithCurrency = Just $ PriceAPIEntity securityDepositAmount dfee.currency

      let refundStatuses = [DDF.REFUNDED, DDF.REFUND_FAILED]
      if dfee.status `elem` refundStatuses
        then
          return
            [ SecurityDepositDfStatusRes
                { securityDepositStatus = DDF.CLEARED,
                  securityDepositAmountWithCurrency = securityDepositAmountWithCurrency,
                  refundedAmount = Nothing,
                  driverFeeId = dfee.id.getId,
                  createdAt = dfee.createdAt
                },
              SecurityDepositDfStatusRes
                { securityDepositStatus = dfee.status,
                  securityDepositAmountWithCurrency = securityDepositAmountWithCurrency,
                  refundedAmount = dfee.refundedAmount,
                  driverFeeId = dfee.id.getId,
                  createdAt = fromMaybe dfee.updatedAt dfee.refundedAt
                }
            ]
        else
          return
            [ SecurityDepositDfStatusRes
                { securityDepositStatus = dfee.status,
                  securityDepositAmountWithCurrency = securityDepositAmountWithCurrency,
                  refundedAmount = dfee.refundedAmount,
                  driverFeeId = dfee.id.getId,
                  createdAt = dfee.createdAt
                }
            ]

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
  when (refundByPayoutReq.refundAmountDeduction < 0.0) $ throwError (InternalError "Repair charge is less than 0")
  Redis.whenWithLockRedis (mkPayoutLockKeyByDriverAndService personId serviceName) 60 $ do
    let driverFeeType = refundByPayoutReq.driverFeeType
        refundAmountDeduction = refundByPayoutReq.refundAmountDeduction
    (_, mDriverPlan) <- DAPlan.getSubcriptionStatusWithPlan serviceName personId
    driverInfo <- QDriverInformation.findById personId >>= fromMaybeM DriverInfoNotFound
    ------------ todo :-  put check for access post rbac implemtation --------------
    let mbVpa = refundByPayoutReq.payerVpa <|> driverInfo.payoutVpa <|> (mDriverPlan >>= (.payerVpa))
    unless (isJust mbVpa) $ throwError (InternalError $ "payer vpa not present for " <> personId.getId)
    whenJust mbVpa $ \vpa -> do
      pendingDriverFees <- runInReplica $ QDF.findAllFeeByTypeServiceStatusAndDriver serviceName personId [DDF.RECURRING_EXECUTION_INVOICE] [DDF.PAYMENT_PENDING]
      unless (null pendingDriverFees) $ throwError (InternalError "some driver fee currently in auto pay execution")
      driverFees <- runInReplica $ QDF.findAllFeeByTypeServiceStatusAndDriver serviceName personId [driverFeeType] [DDF.CLEARED, DDF.REFUND_FAILED, DDF.COLLECTED_CASH]
      dueDriverFees <- QDF.findAllByStatusAndDriverIdWithServiceName personId [DDF.PAYMENT_OVERDUE] Nothing serviceName
      let totalSecurityDeposit = sum $ map mapToAmount driverFees
          dueDriverFeesAmount = sum $ map mapToAmount dueDriverFees
          refundAmount = totalSecurityDeposit - dueDriverFeesAmount - refundAmountDeduction
      when (refundAmount < 0.0) $ throwError (InternalError "refund amount is less than 0")
      let driverFeeSorted = sortOn (.platformFee.fee) driverFees
      subscriptionConfig <- do
        CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCityId Nothing serviceName
          >>= fromMaybeM (NoSubscriptionConfigForService opCityId.getId $ show serviceName)
      uid <- generateGUID
      now <- getCurrentTime
      let ((driverFeeToPayout, driverFeeToSettle), _) = driverFeeWithRefundData driverFeeSorted refundAmount uid now
      person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      phoneNo <- mapM decrypt person.mobileNumber
      payoutServiceName <- Payout.decidePayoutService (fromMaybe (DEMSC.PayoutService TPayout.Juspay) subscriptionConfig.payoutServiceName) person.clientSdkVersion person.merchantOperatingCityId
      let createPayoutOrderReq = mkPayoutReq driverFeeToPayout person vpa uid phoneNo
          entityName = DPayment.DRIVER_FEE
          createPayoutOrderCall = Payout.createPayoutOrder person.merchantId opCityId payoutServiceName (Just person.id.getId)
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
    driverFeeWithRefundData driverFeeSorted refundAmount uid refundInitiatedAt =
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
                          refundedAt = Just refundInitiatedAt,
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
          customerVpa = vpa,
          isDynamicWebhookRequired = False
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
    subscriptionDown :: Maybe Bool,
    vehicleVariantsDisabledForSubscription :: Maybe [VehicleVariant]
  }
  deriving (Generic, Show, Eq, Ord)

getDriverSpecificSubscriptionDataWithSubsConfig ::
  (EsqDBFlow m r, CacheFlow m r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  TransporterConfig ->
  DriverInformation ->
  Maybe Vehicle ->
  Plan.ServiceNames ->
  m DriverSpecificSubscriptionData
getDriverSpecificSubscriptionDataWithSubsConfig (personId, _, opCityId) transporterConfig driverInfo mbVehicle serviceName = do
  let mbVehicleCategory = mbVehicle >>= (.category)
  subscriptionConfig <- CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName opCityId Nothing serviceName
  (autoPayStatus, mbDriverPlan) <- DAPlan.getSubcriptionStatusWithPlan serviceName personId
  freeTrialDaysLeft <- if isFreeTrialEnabled subscriptionConfig then getFreeTrialDaysLeft transporterConfig.freeTrialDays driverInfo else return 0
  let freeTrialDays = if isFreeTrialEnabled subscriptionConfig then transporterConfig.freeTrialDays else 0
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
      vehicleVariantsDisabledForSubscription = subscriptionConfig >>= (.disabledVariantsForSubscription)
  return $ DriverSpecificSubscriptionData {..}
  where
    isFreeTrialEnabled subscriptionConfig = (subscriptionConfig <&> (.isFreeTrialDaysApplicable)) == Just True

consentResponse :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> GetConsentReq -> Flow APISuccess
consentResponse (personId, _, _) req = do
  logInfo $ "Driver consent request - Driver ID: " <> personId.getId <> ", Consent: " <> show req.consent
  QPerson.updateNyClubConsent (Just req.consent) personId
  pure APISuccess.Success

-- | Returns all-time stats for a driver (for /driver/stats/alltime endpoint)
getStatsAllTime ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  m DCommon.DriverStatsRes
getStatsAllTime (driverId, _, merchantOpCityId) = findOnboardedDriversOrFleets driverId merchantOpCityId Nothing Nothing

-- TODO: Need to implement clickhouse aggregated query to fetch data for the given date range
findOnboardedDriversOrFleets :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id SP.Person -> Id DMOC.MerchantOperatingCity -> Maybe Day -> Maybe Day -> m DCommon.DriverStatsRes
findOnboardedDriversOrFleets personId merchantOpCityId maybeFrom maybeTo = do
  currency <- SMerchant.getCurrencyByMerchantOpCity merchantOpCityId
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  DOnlineDuration.updateOnlineDurationDuringFetchingDailyStats personId transporterConfig
  let defaultStats =
        DCommon.DriverStatsRes
          { numDriversOnboarded = 0,
            numFleetsOnboarded = 0,
            totalRides = 0,
            totalEarnings = Money 0,
            totalDistance = Meters 0,
            bonusEarnings = Money 0,
            totalEarningsWithCurrency = PriceAPIEntity 0.0 currency,
            totalEarningsPerKm = Money 0,
            totalEarningsPerKmWithCurrency = PriceAPIEntity 0.0 currency,
            bonusEarningsWithCurrency = PriceAPIEntity 0.0 currency,
            onlineDuration = Seconds 0
          }
  let earningsWindowSize = transporterConfig.analyticsConfig.earningsWindowSize
  case (maybeFrom, maybeTo) of
    (Nothing, Nothing) -> do
      stats <- runInReplica $ QDriverStats.findByPrimaryKey personId >>= fromMaybeM (InternalError $ "Driver Stats data not found for entity " <> show personId.getId)
      let totalEarningsPerKm = calculateEarningsPerKm stats.totalDistance stats.totalEarnings
      return $
        DCommon.DriverStatsRes
          { numDriversOnboarded = stats.numDriversOnboarded,
            numFleetsOnboarded = stats.numFleetsOnboarded,
            totalRides = stats.totalRides,
            totalEarnings = roundToIntegral stats.totalEarnings,
            totalDistance = stats.totalDistance,
            bonusEarnings = roundToIntegral stats.bonusEarned,
            totalEarningsWithCurrency = PriceAPIEntity stats.totalEarnings currency,
            totalEarningsPerKm = roundToIntegral totalEarningsPerKm,
            totalEarningsPerKmWithCurrency = PriceAPIEntity totalEarningsPerKm currency,
            bonusEarningsWithCurrency = PriceAPIEntity stats.bonusEarned currency,
            onlineDuration = stats.onlineDuration
          }
    (Just fromDate, Just toDate) | fromDate == toDate -> do
      mbStats <- runInReplica $ SQDS.findByDriverIdAndDate personId fromDate
      case mbStats of
        Nothing -> return defaultStats
        Just stats -> do
          let totalEarningsPerKm = calculateEarningsPerKm stats.totalDistance stats.totalEarnings
          return $
            DCommon.DriverStatsRes
              { numDriversOnboarded = stats.numDriversOnboarded,
                numFleetsOnboarded = stats.numFleetsOnboarded,
                totalRides = stats.numRides,
                totalEarnings = roundToIntegral stats.totalEarnings,
                totalDistance = stats.totalDistance,
                bonusEarnings = roundToIntegral stats.bonusEarnings,
                totalEarningsWithCurrency = PriceAPIEntity stats.totalEarnings currency,
                totalEarningsPerKm = roundToIntegral totalEarningsPerKm,
                totalEarningsPerKmWithCurrency = PriceAPIEntity totalEarningsPerKm currency,
                bonusEarningsWithCurrency = PriceAPIEntity stats.bonusEarnings currency,
                onlineDuration = fromMaybe (Seconds 0) stats.onlineDuration
              }
    (Just fromDate, Just toDate) | fromIntegral (diffDays toDate fromDate) <= earningsWindowSize -> do
      statsList <- runInReplica $ SQDS.findAllInRangeByDriverId_ personId fromDate toDate
      if null statsList
        then return defaultStats
        else do
          let agg f = sum (map f statsList)
              aggMoney f = HighPrecMoney $ sum (map (getHighPrecMoney . f) statsList)
              aggMeters f = Meters $ sum (map (getMeters . f) statsList)
              aggMbSeconds f = Seconds $ sum (map (getSeconds . fromMaybe (Seconds 0) . f) statsList)
              totalEarnings = aggMoney (.totalEarnings)
              totalDistance = aggMeters (.totalDistance)
              bonusEarnings = aggMoney (.bonusEarnings)
              totalEarningsPerKm = calculateEarningsPerKm totalDistance totalEarnings
          return $
            DCommon.DriverStatsRes
              { numDriversOnboarded = agg (.numDriversOnboarded),
                numFleetsOnboarded = agg (.numFleetsOnboarded),
                totalRides = agg (.numRides),
                totalEarnings = roundToIntegral totalEarnings,
                totalDistance = totalDistance,
                bonusEarnings = roundToIntegral bonusEarnings,
                totalEarningsWithCurrency = PriceAPIEntity totalEarnings currency,
                totalEarningsPerKm = roundToIntegral totalEarningsPerKm,
                totalEarningsPerKmWithCurrency = PriceAPIEntity totalEarningsPerKm currency,
                bonusEarningsWithCurrency = PriceAPIEntity bonusEarnings currency,
                onlineDuration = aggMbSeconds (.onlineDuration)
              }
    _ -> throwError (InvalidRequest "Invalid date range: Dates must be empty, same day, or max 7 days apart.")
  where
    calculateEarningsPerKm :: Meters -> HighPrecMoney -> HighPrecMoney
    calculateEarningsPerKm distance earnings =
      let distanceInKm = distance `div` 1000
       in if distanceInKm.getMeters == 0
            then HighPrecMoney 0.0
            else toHighPrecMoney $ roundToIntegral earnings `div` distanceInKm.getMeters
