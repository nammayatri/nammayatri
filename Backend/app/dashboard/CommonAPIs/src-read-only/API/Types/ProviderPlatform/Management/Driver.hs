{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Driver where

import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Dashboard.ProviderPlatform.Fleet.Driver
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Time.Calendar
import qualified Domain.Types.ServiceTierType
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Kernel.Types.Version
import Servant
import Servant.Client

data AadhaarDetails = AadhaarDetails {aadhaarStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text, aadhaarStatusTime :: Kernel.Prelude.UTCTime, aadhaarTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AadharPanSyncReq = AadharPanSyncReq {phoneNo :: Kernel.Prelude.Text, countryCode :: Kernel.Prelude.Text, documentType :: API.Types.ProviderPlatform.Management.Driver.SyncDocType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AadharPanSyncReq where
  hideSecrets = Kernel.Prelude.identity

data BlockDriverWithReasonReq = BlockDriverWithReasonReq {reasonCode :: Kernel.Prelude.Text, blockReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text, blockTimeInHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BlockReason = BlockReason
  { reasonCode :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Management.Driver.BlockReason,
    blockReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blockTimeInHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype ChangeOperatingCityReq = ChangeOperatingCityReq {operatingCity :: Kernel.Types.Beckn.Context.City}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ChangeOperatingCityReq where
  hideSecrets = Kernel.Prelude.identity

data ClearDriverFeeReq = ClearDriverFeeReq
  { serviceName :: Dashboard.Common.Driver.ServiceNames,
    feeType :: API.Types.ProviderPlatform.Management.Driver.DriverFeeType,
    platformFee :: Kernel.Types.Common.HighPrecMoney,
    sgstPercentage :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    cgstPercentage :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Prelude.Maybe Kernel.Types.Common.Currency,
    sendManualLink :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype ClearOnRideStuckDriversRes = ClearOnRideStuckDriversRes {driverIds :: [Kernel.Types.Id.Id Dashboard.Common.Driver]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype DeleteRCReq = DeleteRCReq {rcNo :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DeleteRCReq where
  hideSecrets = Kernel.Prelude.identity

type DriverAadhaarInfoByPhoneReq = API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoRes

data DriverAadhaarInfoRes = DriverAadhaarInfoRes {driverName :: Kernel.Prelude.Text, driverGender :: Kernel.Prelude.Text, driverDob :: Kernel.Prelude.Text, driverImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverFeeType
  = PAYOUT_REGISTRATION
  | ONE_TIME_SECURITY_DEPOSIT
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data DriverListItem = DriverListItem
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    firstName :: Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    phoneNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    blocked :: Kernel.Prelude.Bool,
    subscribed :: Kernel.Prelude.Bool,
    verified :: Kernel.Prelude.Bool,
    onRide :: Kernel.Prelude.Bool,
    active :: Kernel.Prelude.Bool,
    onboardingDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverListRes = DriverListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, drivers :: [API.Types.ProviderPlatform.Management.Driver.DriverListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverLocationItem = DriverLocationItem
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    firstName :: Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNo :: Kernel.Prelude.Text,
    phoneNo :: Kernel.Prelude.Text,
    active :: Kernel.Prelude.Bool,
    onRide :: Kernel.Prelude.Bool,
    location :: Kernel.External.Maps.Types.LatLong,
    lastLocationTimestamp :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverLocationRes = DriverLocationRes
  { driverLocationsNotFound :: Kernel.Prelude.Maybe (Kernel.Prelude.NonEmpty (Kernel.Types.Id.Id Dashboard.Common.Driver)),
    driverLocations :: [API.Types.ProviderPlatform.Management.Driver.DriverLocationItem]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype GetOperatingCityResp = GetOperatingCityResp {operatingCity :: Kernel.Types.Beckn.Context.City}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets GetOperatingCityResp where
  hideSecrets = Kernel.Prelude.identity

data LicDetails = LicDetails {licExpiry :: Kernel.Prelude.UTCTime, vehicleClass :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NotificationSource
  = FCM
  | GRPC
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data PanAadharSelfieDetailsResp = PanAadharSelfieDetailsResp
  { personName :: Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Text,
    selfieDetails :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Driver.SelfieDetails,
    aadhaarDetails :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Driver.AadhaarDetails,
    panDetails :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Driver.PanDetails
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PanDetails = PanDetails {panStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text, panStatusTime :: Kernel.Prelude.UTCTime, panTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PauseOrResumeServiceChargesReq = PauseOrResumeServiceChargesReq
  { serviceChargeEligibility :: Kernel.Prelude.Bool,
    vehicleId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceName :: Dashboard.Common.Driver.ServiceNames,
    reason :: Kernel.Prelude.Maybe Dashboard.Common.Driver.ReasonForDisablingServiceCharge,
    planId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets PauseOrResumeServiceChargesReq where
  hideSecrets = Kernel.Prelude.identity

data RCDetails = RCDetails {vehicleClass :: Kernel.Prelude.Text, fitnessExpiry :: Kernel.Prelude.UTCTime, insuranceExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RefundByPayoutReq = RefundByPayoutReq
  { serviceName :: Dashboard.Common.Driver.ServiceNames,
    refundAmountDeduction :: Kernel.Types.Common.HighPrecMoney,
    payerVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverFeeType :: API.Types.ProviderPlatform.Management.Driver.DriverFeeType,
    refundAmountSegregation :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RefundByPayoutReq where
  hideSecrets = Kernel.Prelude.identity

data ReviewRCVariantReq = ReviewRCVariantReq {rcId :: Kernel.Prelude.Text, vehicleVariant :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant, markReviewed :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReviewRCVariantRes = ReviewRCVariantRes {rcId :: Kernel.Prelude.Text, status :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SearchRequestForDriver = SearchRequestForDriver
  { acceptanceRatio :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    actualDistanceToPickup :: Kernel.Types.Common.Meters,
    airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    baseFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    batchNumber :: Kernel.Prelude.Int,
    cancellationRatio :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    customerCancellationDues :: Kernel.Types.Common.HighPrecMoney,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverAvailableTime :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    driverDefaultStepFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    driverMaxExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverMinExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverSpeed :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    driverStepFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    durationToPickup :: Kernel.Types.Common.Seconds,
    estimateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    goHomeRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.DriverGoHomeRequest),
    id :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Management.Driver.SearchRequestForDriver,
    isForwardRequest :: Kernel.Prelude.Bool,
    isPartOfIntelligentPool :: Kernel.Prelude.Bool,
    keepHiddenForSeconds :: Kernel.Types.Common.Seconds,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Merchant),
    merchantOperatingCityId :: Kernel.Types.Id.Id Dashboard.Common.MerchantOperatingCity,
    mode :: Kernel.Prelude.Maybe Dashboard.ProviderPlatform.Fleet.Driver.DriverMode,
    notificationSource :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Driver.NotificationSource,
    parallelSearchRequestCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    pickupZone :: Kernel.Prelude.Bool,
    previousDropGeoHash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    renderedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    requestId :: Kernel.Types.Id.Id Dashboard.Common.SearchRequest,
    respondedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    response :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Driver.SearchRequestForDriverResponse,
    rideFrequencyScore :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    rideRequestPopupDelayDuration :: Kernel.Types.Common.Seconds,
    searchRequestValidTill :: Kernel.Prelude.UTCTime,
    searchTryId :: Kernel.Types.Id.Id Dashboard.Common.SearchTry,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Kernel.Prelude.Text,
    straightLineDistanceToPickup :: Kernel.Types.Common.Meters,
    totalRides :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vehicleAge :: Kernel.Prelude.Maybe Kernel.Types.Time.Months,
    vehicleServiceTier :: Domain.Types.ServiceTierType.ServiceTierType,
    vehicleServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleVariant :: Dashboard.Common.VehicleVariant
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets SearchRequestForDriver where
  hideSecrets = Kernel.Prelude.identity

data SearchRequestForDriverResponse
  = Accept
  | Reject
  | Pulled
  deriving stock (Eq, Show, Generic, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data SecurityDepositDfStatusRes = SecurityDepositDfStatusRes
  { securityDepositStatus :: Dashboard.Common.Driver.DriverFeeStatus,
    securityDepositAmountWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    driverFeeId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SelfieDetails = SelfieDetails {latestStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text, latestStatusTime :: Kernel.Prelude.UTCTime, latestTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SyncDocType
  = Aadhaar
  | Pan
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateACUsageRestrictionReq = UpdateACUsageRestrictionReq {isWorking :: Kernel.Prelude.Bool, downgradeReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateACUsageRestrictionReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateDriverDataReq = UpdateDriverDataReq
  { driverName :: Kernel.Prelude.Text,
    driverGender :: Kernel.Prelude.Text,
    driverDob :: Kernel.Prelude.Text,
    driverAadhaarNumber :: Kernel.Prelude.Text,
    isVerified :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateDriverNameReq = UpdateDriverNameReq {firstName :: Kernel.Prelude.Text, middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text, lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateDriverNameReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateDriverTagReq = UpdateDriverTagReq {driverTag :: Kernel.Prelude.Text, isAddingTag :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateDriverTagReq where
  hideSecrets = Kernel.Prelude.identity

data UpdatePhoneNumberReq = UpdatePhoneNumberReq {newPhoneNumber :: Kernel.Prelude.Text, newCountryCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdatePhoneNumberReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateRCInvalidStatusReq = UpdateRCInvalidStatusReq {rcId :: Kernel.Prelude.Text, vehicleVariant :: Dashboard.Common.VehicleVariant}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateRCInvalidStatusReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateVehicleManufacturingReq = UpdateVehicleManufacturingReq {rcId :: Kernel.Prelude.Text, manufacturing :: Data.Time.Calendar.Day}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateVehicleManufacturingReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateVehicleVariantReq = UpdateVehicleVariantReq {rcId :: Kernel.Prelude.Text, vehicleVariant :: Dashboard.Common.VehicleVariant}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateVehicleVariantReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("driver" :> (GetDriverDocumentsInfo :<|> PostDriverPersonNumbers :<|> PostDriverPersonId :<|> GetDriverAadhaarInfo :<|> GetDriverAadhaarInfobyMobileNumber :<|> GetDriverList :<|> GetDriverActivity :<|> PostDriverDisable :<|> PostDriverAcRestrictionUpdate :<|> PostDriverBlockWithReasonHelper :<|> PostDriverBlock :<|> GetDriverBlockReasonList :<|> PostDriverUnblockHelper :<|> GetDriverLocation :<|> DeleteDriverPermanentlyDelete :<|> PostDriverUnlinkDL :<|> PostDriverUnlinkAadhaar :<|> PostDriverUpdatePhoneNumber :<|> PostDriverUpdateByPhoneNumber :<|> PostDriverUpdateName :<|> PostDriverDeleteRC :<|> GetDriverClearStuckOnRide :<|> PostDriverSendDummyNotification :<|> PostDriverChangeOperatingCity :<|> GetDriverGetOperatingCity :<|> PostDriverPauseOrResumeServiceCharges :<|> PostDriverUpdateRCInvalidStatus :<|> PostDriverUpdateVehicleVariant :<|> PostDriverBulkReviewRCVariant :<|> PostDriverUpdateDriverTag :<|> PostDriverClearFee :<|> GetDriverPanAadharSelfieDetails :<|> PostDriverSyncDocAadharPan :<|> PostDriverUpdateVehicleManufacturing :<|> PostDriverRefundByPayout :<|> GetDriverSecurityDepositStatus :<|> GetDriverDriverLicenseDetails :<|> GetDriverSearchRequests))

type GetDriverDocumentsInfo = ("documents" :> "info" :> Get '[JSON] Dashboard.Common.Driver.DriverDocumentsInfoRes)

type PostDriverPersonNumbers = ("personNumbers" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.PersonIdsReq :> Post '[JSON] [Dashboard.Common.PersonRes])

type PostDriverPersonId = ("personId" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.PersonMobileNoReq :> Post '[JSON] [Dashboard.Common.PersonRes])

type GetDriverAadhaarInfo = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "aadhaarInfo" :> Get '[JSON] API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoRes)

type GetDriverAadhaarInfobyMobileNumber =
  ( Capture "mobileNo" Kernel.Prelude.Text :> "aadhaarInfobyMobileNumber"
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoByPhoneReq
  )

type GetDriverList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "verified" Kernel.Prelude.Bool
      :> QueryParam
           "enabled"
           Kernel.Prelude.Bool
      :> QueryParam "blocked" Kernel.Prelude.Bool
      :> QueryParam
           "subscribed"
           Kernel.Prelude.Bool
      :> QueryParam
           "phone"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleNumberSearchString"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.DriverListRes
  )

type GetDriverActivity = ("activity" :> Get '[JSON] Dashboard.Common.Driver.DriverActivityRes)

type PostDriverDisable = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "disable" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverAcRestrictionUpdate =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "acRestriction" :> "update"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateACUsageRestrictionReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverBlockWithReason =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "blockWithReason"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.BlockDriverWithReasonReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverBlockWithReasonHelper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "blockWithReason" :> Capture "dashboardUserName" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.BlockDriverWithReasonReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverBlock = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "block" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetDriverBlockReasonList = ("blockReasonList" :> Get '[JSON] [API.Types.ProviderPlatform.Management.Driver.BlockReason])

type PostDriverUnblock = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unblock" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUnblockHelper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unblock" :> Capture "dashboardUserName" Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverLocation =
  ( "location" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> ReqBody '[JSON] Dashboard.Common.Driver.DriverIds
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.DriverLocationRes
  )

type DeleteDriverPermanentlyDelete = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "permanentlyDelete" :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUnlinkDL = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unlinkDL" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUnlinkAadhaar = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unlinkAadhaar" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUpdatePhoneNumber =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updatePhoneNumber"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdatePhoneNumberReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateByPhoneNumber =
  ( Capture "mobileNo" Kernel.Prelude.Text :> "updateByPhoneNumber"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateDriverDataReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateName =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updateName"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateDriverNameReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverDeleteRC =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "deleteRC" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Driver.DeleteRCReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverClearStuckOnRide = ("clearStuck" :> "onRide" :> QueryParam "dbSyncTime" Kernel.Prelude.Int :> Get '[JSON] API.Types.ProviderPlatform.Management.Driver.ClearOnRideStuckDriversRes)

type PostDriverSendDummyNotification = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "sendDummyNotification" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverChangeOperatingCity =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "changeOperatingCity"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.ChangeOperatingCityReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetDriverGetOperatingCity =
  ( "getOperatingCity" :> QueryParam "mobileCountryCode" Kernel.Prelude.Text :> QueryParam "mobileNumber" Kernel.Prelude.Text
      :> QueryParam
           "rideId"
           (Kernel.Types.Id.Id Dashboard.Common.Ride)
      :> Get '[JSON] API.Types.ProviderPlatform.Management.Driver.GetOperatingCityResp
  )

type PostDriverPauseOrResumeServiceCharges =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "pauseOrResumeServiceCharges"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.PauseOrResumeServiceChargesReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateRCInvalidStatus =
  ( "updateRCInvalidStatus" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateRCInvalidStatusReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateVehicleVariant =
  ( "updateVehicleVariant" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateVehicleVariantReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverBulkReviewRCVariant =
  ( "bulkReviewRCVariant" :> ReqBody '[JSON] [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantReq]
      :> Post
           '[JSON]
           [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantRes]
  )

type PostDriverUpdateDriverTag =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updateDriverTag"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateDriverTagReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverClearFee =
  ( "clearFee" :> MandatoryQueryParam "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetDriverPanAadharSelfieDetails =
  ( "panAadharSelfieDetails" :> MandatoryQueryParam "countryCode" Kernel.Prelude.Text :> MandatoryQueryParam "phoneNo" Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.PanAadharSelfieDetailsResp
  )

type PostDriverSyncDocAadharPan = ("syncDocAadharPan" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Driver.AadharPanSyncReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUpdateVehicleManufacturing =
  ( "updateVehicleManufacturing" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.UpdateVehicleManufacturingReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverRefundByPayout =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "refundByPayout"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.Driver.RefundByPayoutReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetDriverSecurityDepositStatus =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "securityDepositStatus"
      :> QueryParam
           "serviceName"
           Dashboard.Common.Driver.ServiceNames
      :> Get '[JSON] [API.Types.ProviderPlatform.Management.Driver.SecurityDepositDfStatusRes]
  )

type GetDriverDriverLicenseDetails = (Capture "licenseId" (Kernel.Types.Id.Id Dashboard.Common.Driver.DriverLicense) :> "DriverLicenseDetails" :> Get '[JSON] Dashboard.Common.Driver.DriverLicenseD)

type GetDriverSearchRequests =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> Capture "xMin" Kernel.Prelude.Int :> "searchRequests"
      :> Get
           '[JSON]
           [API.Types.ProviderPlatform.Management.Driver.SearchRequestForDriver]
  )

data DriverAPIs = DriverAPIs
  { getDriverDocumentsInfo :: EulerHS.Types.EulerClient Dashboard.Common.Driver.DriverDocumentsInfoRes,
    postDriverPersonNumbers :: (Data.ByteString.Lazy.ByteString, Dashboard.Common.PersonIdsReq) -> EulerHS.Types.EulerClient [Dashboard.Common.PersonRes],
    postDriverPersonId :: (Data.ByteString.Lazy.ByteString, Dashboard.Common.PersonMobileNoReq) -> EulerHS.Types.EulerClient [Dashboard.Common.PersonRes],
    getDriverAadhaarInfo :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoRes,
    getDriverAadhaarInfobyMobileNumber :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.DriverAadhaarInfoByPhoneReq,
    getDriverList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.DriverListRes,
    getDriverActivity :: EulerHS.Types.EulerClient Dashboard.Common.Driver.DriverActivityRes,
    postDriverDisable :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverAcRestrictionUpdate :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateACUsageRestrictionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverBlockWithReason :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Driver.BlockDriverWithReasonReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverBlock :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverBlockReasonList :: EulerHS.Types.EulerClient [API.Types.ProviderPlatform.Management.Driver.BlockReason],
    postDriverUnblock :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverLocation :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Dashboard.Common.Driver.DriverIds -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.DriverLocationRes,
    deleteDriverPermanentlyDelete :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUnlinkDL :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUnlinkAadhaar :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdatePhoneNumber :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdatePhoneNumberReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateByPhoneNumber :: Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverDataReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateName :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverNameReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverDeleteRC :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.DeleteRCReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverClearStuckOnRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.ClearOnRideStuckDriversRes,
    postDriverSendDummyNotification :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverChangeOperatingCity :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ChangeOperatingCityReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverGetOperatingCity :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.GetOperatingCityResp,
    postDriverPauseOrResumeServiceCharges :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.PauseOrResumeServiceChargesReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateRCInvalidStatus :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateRCInvalidStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateVehicleVariant :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateVehicleVariantReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverBulkReviewRCVariant :: [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantReq] -> EulerHS.Types.EulerClient [API.Types.ProviderPlatform.Management.Driver.ReviewRCVariantRes],
    postDriverUpdateDriverTag :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateDriverTagReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverClearFee :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverPanAadharSelfieDetails :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.PanAadharSelfieDetailsResp,
    postDriverSyncDocAadharPan :: API.Types.ProviderPlatform.Management.Driver.AadharPanSyncReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateVehicleManufacturing :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.UpdateVehicleManufacturingReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverRefundByPayout :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.Driver.RefundByPayoutReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverSecurityDepositStatus :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Dashboard.Common.Driver.ServiceNames -> EulerHS.Types.EulerClient [API.Types.ProviderPlatform.Management.Driver.SecurityDepositDfStatusRes],
    getDriverDriverLicenseDetails :: Kernel.Types.Id.Id Dashboard.Common.Driver.DriverLicense -> EulerHS.Types.EulerClient Dashboard.Common.Driver.DriverLicenseD,
    getDriverSearchRequests :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Int -> EulerHS.Types.EulerClient [API.Types.ProviderPlatform.Management.Driver.SearchRequestForDriver]
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    getDriverDocumentsInfo :<|> postDriverPersonNumbers :<|> postDriverPersonId :<|> getDriverAadhaarInfo :<|> getDriverAadhaarInfobyMobileNumber :<|> getDriverList :<|> getDriverActivity :<|> postDriverDisable :<|> postDriverAcRestrictionUpdate :<|> postDriverBlockWithReason :<|> postDriverBlock :<|> getDriverBlockReasonList :<|> postDriverUnblock :<|> getDriverLocation :<|> deleteDriverPermanentlyDelete :<|> postDriverUnlinkDL :<|> postDriverUnlinkAadhaar :<|> postDriverUpdatePhoneNumber :<|> postDriverUpdateByPhoneNumber :<|> postDriverUpdateName :<|> postDriverDeleteRC :<|> getDriverClearStuckOnRide :<|> postDriverSendDummyNotification :<|> postDriverChangeOperatingCity :<|> getDriverGetOperatingCity :<|> postDriverPauseOrResumeServiceCharges :<|> postDriverUpdateRCInvalidStatus :<|> postDriverUpdateVehicleVariant :<|> postDriverBulkReviewRCVariant :<|> postDriverUpdateDriverTag :<|> postDriverClearFee :<|> getDriverPanAadharSelfieDetails :<|> postDriverSyncDocAadharPan :<|> postDriverUpdateVehicleManufacturing :<|> postDriverRefundByPayout :<|> getDriverSecurityDepositStatus :<|> getDriverDriverLicenseDetails :<|> getDriverSearchRequests = driverClient

data DriverEndpointDSL
  = GetDriverDocumentsInfoEndpoint
  | PostDriverPersonNumbersEndpoint
  | PostDriverPersonIdEndpoint
  | GetDriverAadhaarInfoEndpoint
  | GetDriverAadhaarInfobyMobileNumberEndpoint
  | GetDriverListEndpoint
  | GetDriverActivityEndpoint
  | PostDriverDisableEndpoint
  | PostDriverAcRestrictionUpdateEndpoint
  | PostDriverBlockWithReasonEndpoint
  | PostDriverBlockEndpoint
  | GetDriverBlockReasonListEndpoint
  | PostDriverUnblockEndpoint
  | GetDriverLocationEndpoint
  | DeleteDriverPermanentlyDeleteEndpoint
  | PostDriverUnlinkDLEndpoint
  | PostDriverUnlinkAadhaarEndpoint
  | PostDriverUpdatePhoneNumberEndpoint
  | PostDriverUpdateByPhoneNumberEndpoint
  | PostDriverUpdateNameEndpoint
  | PostDriverDeleteRCEndpoint
  | GetDriverClearStuckOnRideEndpoint
  | PostDriverSendDummyNotificationEndpoint
  | PostDriverChangeOperatingCityEndpoint
  | GetDriverGetOperatingCityEndpoint
  | PostDriverPauseOrResumeServiceChargesEndpoint
  | PostDriverUpdateRCInvalidStatusEndpoint
  | PostDriverUpdateVehicleVariantEndpoint
  | PostDriverBulkReviewRCVariantEndpoint
  | PostDriverUpdateDriverTagEndpoint
  | PostDriverClearFeeEndpoint
  | GetDriverPanAadharSelfieDetailsEndpoint
  | PostDriverSyncDocAadharPanEndpoint
  | PostDriverUpdateVehicleManufacturingEndpoint
  | PostDriverRefundByPayoutEndpoint
  | GetDriverSecurityDepositStatusEndpoint
  | GetDriverDriverLicenseDetailsEndpoint
  | GetDriverSearchRequestsEndpoint
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
