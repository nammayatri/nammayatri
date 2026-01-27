{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Driver where

import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time.Calendar
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
import qualified Lib.Yudhishthira.Types
import Servant
import Servant.Client

data AadhaarDetails = AadhaarDetails {aadhaarStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text, aadhaarStatusTime :: Kernel.Prelude.UTCTime, aadhaarTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AadharPanSyncReq = AadharPanSyncReq {phoneNo :: Kernel.Prelude.Text, countryCode :: Kernel.Prelude.Text, documentType :: SyncDocType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AadharPanSyncReq where
  hideSecrets = Kernel.Prelude.identity

data AppendSelectedServiceTiersReq = AppendSelectedServiceTiersReq {selected_service_tiers :: [Dashboard.Common.ServiceTierType]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AppendSelectedServiceTiersReq where
  hideSecrets = Kernel.Prelude.identity

data BlockDriverWithReasonReq = BlockDriverWithReasonReq {reasonCode :: Kernel.Prelude.Text, blockReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text, blockTimeInHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BlockReason = BlockReason {reasonCode :: Kernel.Types.Id.Id BlockReason, blockReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text, blockTimeInHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BulkServiceUpdateReq = BulkServiceUpdateReq {driverIds :: [Kernel.Prelude.Text], serviceNames :: [Dashboard.Common.Driver.ServiceNames]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets BulkServiceUpdateReq where
  hideSecrets = Kernel.Prelude.identity

newtype ChangeOperatingCityReq = ChangeOperatingCityReq {operatingCity :: Kernel.Types.Beckn.Context.City}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ChangeOperatingCityReq where
  hideSecrets = Kernel.Prelude.identity

data ClearDriverFeeReq = ClearDriverFeeReq
  { serviceName :: Dashboard.Common.Driver.ServiceNames,
    feeType :: DriverFeeType,
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

type DriverAadhaarInfoByPhoneReq = DriverAadhaarInfoRes

data DriverAadhaarInfoRes = DriverAadhaarInfoRes {driverName :: Kernel.Prelude.Text, driverGender :: Kernel.Prelude.Text, driverDob :: Kernel.Prelude.Text, driverImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverDecDataResp = DriverDecDataResp {driverIdResp :: Kernel.Types.Id.Id Dashboard.Common.Driver, driverDataResp :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DriverDecDataResp where
  hideSecrets = Kernel.Prelude.identity

data DriverEncDataReq = DriverEncDataReq {driverIdReq :: Kernel.Types.Id.Id Dashboard.Common.Driver, driverDataReq :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DriverEncDataReq where
  hideSecrets = Kernel.Prelude.identity

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

data DriverListRes = DriverListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, drivers :: [DriverListItem]}
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

data DriverLocationRes = DriverLocationRes {driverLocationsNotFound :: Kernel.Prelude.Maybe (Kernel.Prelude.NonEmpty (Kernel.Types.Id.Id Dashboard.Common.Driver)), driverLocations :: [DriverLocationItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverStatsRes = DriverStatsRes
  { numDriversOnboarded :: Kernel.Prelude.Int,
    numFleetsOnboarded :: Kernel.Prelude.Int,
    totalRides :: Kernel.Prelude.Int,
    totalEarnings :: Kernel.Types.Common.Money,
    totalDistance :: Kernel.Types.Common.Meters,
    bonusEarnings :: Kernel.Types.Common.Money,
    totalEarningsWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    totalEarningsPerKm :: Kernel.Types.Common.Money,
    totalEarningsPerKmWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    bonusEarningsWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    onlineDuration :: Kernel.Types.Common.Seconds
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EarningPeriodStats = EarningPeriodStats
  { periodStart :: Data.Time.Calendar.Day,
    totalEarnings :: Kernel.Types.Common.Money,
    totalDistance :: Kernel.Types.Common.Meters,
    totalRides :: Kernel.Prelude.Int,
    cancellationCharges :: Kernel.Types.Common.Money,
    tipAmount :: Kernel.Types.Common.Money
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EarningPeriodStatsRes = EarningPeriodStatsRes {earnings :: [EarningPeriodStats]}
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

data PanAadharSelfieDetailsListResp = PanAadharSelfieDetailsListResp
  { transactionId :: Kernel.Prelude.Text,
    verificationStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    imageId1 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    imageId2 :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PanAadharSelfieDetailsResp = PanAadharSelfieDetailsResp
  { personName :: Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Text,
    selfieDetails :: Kernel.Prelude.Maybe SelfieDetails,
    aadhaarDetails :: Kernel.Prelude.Maybe AadhaarDetails,
    panDetails :: Kernel.Prelude.Maybe PanDetails
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

data RCDetails = RCDetails
  { vehicleRegistrationCertNumber :: Kernel.Prelude.Text,
    vehicleClass :: Kernel.Prelude.Text,
    fitnessExpiry :: Kernel.Prelude.UTCTime,
    insuranceExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    failedRules :: [Kernel.Prelude.Text],
    verificationStatus :: Kernel.Prelude.Maybe Dashboard.Common.VerificationStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RefundByPayoutReq = RefundByPayoutReq
  { serviceName :: Dashboard.Common.Driver.ServiceNames,
    refundAmountDeduction :: Kernel.Types.Common.HighPrecMoney,
    payerVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverFeeType :: DriverFeeType,
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

data SecurityDepositDfStatusRes = SecurityDepositDfStatusRes
  { securityDepositStatus :: Dashboard.Common.Driver.DriverFeeStatus,
    securityDepositAmountWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    driverFeeId :: Kernel.Prelude.Text,
    refundedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
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

data UpdateDriverMerchantReq = UpdateDriverMerchantReq {newMerchantId :: Kernel.Types.Id.Id Dashboard.Common.Merchant, newMerchantOperatingCityId :: Kernel.Types.Id.Id Dashboard.Common.MerchantOperatingCity}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateDriverMerchantReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateDriverNameReq = UpdateDriverNameReq {firstName :: Kernel.Prelude.Text, middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text, lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateDriverNameReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateDriverTagReq = UpdateDriverTagReq {driverTag :: Lib.Yudhishthira.Types.TagNameValue, isAddingTag :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateDriverTagReq where
  hideSecrets = Kernel.Prelude.identity

data UpdatePhoneNumberReq = UpdatePhoneNumberReq {newPhoneNumber :: Kernel.Prelude.Text, newCountryCode :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdatePhoneNumberReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateRCInvalidStatusByRCNumberReq = UpdateRCInvalidStatusByRCNumberReq {rcNumber :: Kernel.Prelude.Text, vehicleVariant :: Dashboard.Common.VehicleVariant}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateRCInvalidStatusByRCNumberReq where
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

type API = ("driver" :> (GetDriverDocumentsInfo :<|> PostDriverPersonNumbers :<|> PostDriverUpdateTagBulk :<|> PostDriverDriverDataDecryption :<|> PostDriverPersonId :<|> GetDriverAadhaarInfo :<|> GetDriverAadhaarInfobyMobileNumber :<|> GetDriverList :<|> GetDriverActivity :<|> PostDriverDisable :<|> PostDriverAcRestrictionUpdate :<|> PostDriverBlockWithReasonHelper :<|> PostDriverBlock :<|> GetDriverBlockReasonList :<|> PostDriverUnblockHelper :<|> GetDriverLocation :<|> DeleteDriverPermanentlyDelete :<|> PostDriverUnlinkDL :<|> PostDriverUnlinkAadhaar :<|> PostDriverUpdatePhoneNumber :<|> PostDriverUpdateByPhoneNumber :<|> PostDriverUpdateName :<|> PostDriverDeleteRC :<|> GetDriverClearStuckOnRide :<|> PostDriverSendDummyNotification :<|> PostDriverChangeOperatingCity :<|> GetDriverGetOperatingCity :<|> PostDriverPauseOrResumeServiceCharges :<|> PostDriverUpdateRCInvalidStatus :<|> PostDriverUpdateRCInvalidStatusByRCNumber :<|> PostDriverUpdateVehicleVariant :<|> PostDriverBulkReviewRCVariant :<|> PostDriverUpdateDriverTag :<|> PostDriverClearFee :<|> GetDriverPanAadharSelfieDetails :<|> PostDriverSyncDocAadharPan :<|> PostDriverUpdateVehicleManufacturing :<|> PostDriverVehicleAppendSelectedServiceTiers :<|> PostDriverVehicleUpsertSelectedServiceTiers :<|> PostDriverRefundByPayout :<|> GetDriverSecurityDepositStatus :<|> GetDriverPanAadharSelfieDetailsList :<|> PostDriverBulkSubscriptionServiceUpdate :<|> GetDriverStatsHelper :<|> GetDriverEarningsHelper :<|> PostDriverUpdateMerchant))

type GetDriverDocumentsInfo = ("documents" :> "info" :> Get '[JSON] Dashboard.Common.Driver.DriverDocumentsInfoRes)

type PostDriverPersonNumbers = ("personNumbers" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.PersonIdsReq :> Post '[JSON] [Dashboard.Common.PersonRes])

type PostDriverUpdateTagBulk =
  ( "updateTagBulk" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.UpdateTagBulkReq
      :> Post
           '[JSON]
           [Dashboard.Common.UpdateTagBulkRes]
  )

type PostDriverDriverDataDecryption = ("driverDataDecryption" :> ReqBody '[JSON] [DriverEncDataReq] :> Post '[JSON] [DriverDecDataResp])

type PostDriverPersonId = ("personId" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp Dashboard.Common.PersonMobileNoReq :> Post '[JSON] [Dashboard.Common.PersonRes])

type GetDriverAadhaarInfo = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "aadhaarInfo" :> Get '[JSON] DriverAadhaarInfoRes)

type GetDriverAadhaarInfobyMobileNumber = (Capture "mobileNo" Kernel.Prelude.Text :> "aadhaarInfobyMobileNumber" :> Get '[JSON] DriverAadhaarInfoByPhoneReq)

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
      :> QueryParam
           "mbNameSearchString"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           DriverListRes
  )

type GetDriverActivity = ("activity" :> Get '[JSON] Dashboard.Common.Driver.DriverActivityRes)

type PostDriverDisable = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "disable" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverAcRestrictionUpdate =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "acRestriction" :> "update" :> ReqBody '[JSON] UpdateACUsageRestrictionReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverBlockWithReason =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "blockWithReason" :> ReqBody '[JSON] BlockDriverWithReasonReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverBlockWithReasonHelper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "blockWithReason" :> Capture "dashboardUserName" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           BlockDriverWithReasonReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverBlock = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "block" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetDriverBlockReasonList = ("blockReasonList" :> Get '[JSON] [BlockReason])

type PostDriverUnblock =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unblock"
      :> QueryParam
           "preventWeeklyCancellationRateBlockingTill"
           Kernel.Prelude.UTCTime
      :> QueryParam "preventDailyCancellationRateBlockingTill" Kernel.Prelude.UTCTime
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUnblockHelper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unblock"
      :> Capture
           "dashboardUserName"
           Kernel.Prelude.Text
      :> QueryParam "preventWeeklyCancellationRateBlockingTill" Kernel.Prelude.UTCTime
      :> QueryParam
           "preventDailyCancellationRateBlockingTill"
           Kernel.Prelude.UTCTime
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverLocation =
  ( "location" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> ReqBody '[JSON] Dashboard.Common.Driver.DriverIds
      :> Get
           '[JSON]
           DriverLocationRes
  )

type DeleteDriverPermanentlyDelete = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "permanentlyDelete" :> Delete '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUnlinkDL = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unlinkDL" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUnlinkAadhaar = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unlinkAadhaar" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUpdatePhoneNumber =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updatePhoneNumber" :> ReqBody '[JSON] UpdatePhoneNumberReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateByPhoneNumber = (Capture "mobileNo" Kernel.Prelude.Text :> "updateByPhoneNumber" :> ReqBody '[JSON] UpdateDriverDataReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUpdateName =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updateName" :> ReqBody '[JSON] UpdateDriverNameReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverDeleteRC = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "deleteRC" :> ReqBody '[JSON] DeleteRCReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetDriverClearStuckOnRide = ("clearStuck" :> "onRide" :> QueryParam "dbSyncTime" Kernel.Prelude.Int :> Get '[JSON] ClearOnRideStuckDriversRes)

type PostDriverSendDummyNotification = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "sendDummyNotification" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverChangeOperatingCity =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "changeOperatingCity" :> ReqBody '[JSON] ChangeOperatingCityReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverGetOperatingCity =
  ( "getOperatingCity" :> QueryParam "mobileCountryCode" Kernel.Prelude.Text :> QueryParam "mobileNumber" Kernel.Prelude.Text
      :> QueryParam
           "rideId"
           (Kernel.Types.Id.Id Dashboard.Common.Ride)
      :> Get '[JSON] GetOperatingCityResp
  )

type PostDriverPauseOrResumeServiceCharges =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "pauseOrResumeServiceCharges"
      :> ReqBody
           '[JSON]
           PauseOrResumeServiceChargesReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateRCInvalidStatus =
  ( "updateRCInvalidStatus" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> ReqBody '[JSON] UpdateRCInvalidStatusReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverUpdateRCInvalidStatusByRCNumber = ("updateRCInvalidStatusByRCNumber" :> ReqBody '[JSON] UpdateRCInvalidStatusByRCNumberReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUpdateVehicleVariant =
  ( "updateVehicleVariant" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> ReqBody '[JSON] UpdateVehicleVariantReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverBulkReviewRCVariant = ("bulkReviewRCVariant" :> ReqBody '[JSON] [ReviewRCVariantReq] :> Post '[JSON] [ReviewRCVariantRes])

type PostDriverUpdateDriverTag =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updateDriverTag" :> ReqBody '[JSON] UpdateDriverTagReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverClearFee =
  ( "clearFee" :> MandatoryQueryParam "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> ReqBody '[JSON] ClearDriverFeeReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverPanAadharSelfieDetails =
  ( "panAadharSelfieDetails" :> MandatoryQueryParam "countryCode" Kernel.Prelude.Text :> MandatoryQueryParam "phoneNo" Kernel.Prelude.Text
      :> Get
           '[JSON]
           PanAadharSelfieDetailsResp
  )

type PostDriverSyncDocAadharPan = ("syncDocAadharPan" :> ReqBody '[JSON] AadharPanSyncReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverUpdateVehicleManufacturing =
  ( "updateVehicleManufacturing" :> Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> ReqBody
           '[JSON]
           UpdateVehicleManufacturingReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverVehicleAppendSelectedServiceTiers =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "vehicle" :> "appendSelectedServiceTiers"
      :> ReqBody
           '[JSON]
           AppendSelectedServiceTiersReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverVehicleUpsertSelectedServiceTiers =
  ( "vehicle" :> "upsertSelectedServiceTiers"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           Dashboard.Common.Driver.UpsertDriverServiceTiersCsvReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverRefundByPayout =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "refundByPayout" :> ReqBody '[JSON] RefundByPayoutReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetDriverSecurityDepositStatus =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "securityDepositStatus"
      :> QueryParam
           "serviceName"
           Dashboard.Common.Driver.ServiceNames
      :> Get '[JSON] [SecurityDepositDfStatusRes]
  )

type GetDriverPanAadharSelfieDetailsList =
  ( "panAadharSelfieDetailsList" :> MandatoryQueryParam "docType" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "driverId"
           (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> Get '[JSON] [PanAadharSelfieDetailsListResp]
  )

type PostDriverBulkSubscriptionServiceUpdate = ("bulk" :> "subscriptionServiceUpdate" :> ReqBody '[JSON] BulkServiceUpdateReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetDriverStats =
  ( "stats" :> QueryParam "entityId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> QueryParam "fromDate" Data.Time.Calendar.Day
      :> QueryParam
           "toDate"
           Data.Time.Calendar.Day
      :> Get '[JSON] DriverStatsRes
  )

type GetDriverStatsHelper =
  ( "stats" :> QueryParam "entityId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> QueryParam "fromDate" Data.Time.Calendar.Day
      :> QueryParam
           "toDate"
           Data.Time.Calendar.Day
      :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Get '[JSON] DriverStatsRes
  )

type GetDriverEarnings =
  ( "earnings" :> MandatoryQueryParam "from" Data.Time.Calendar.Day :> MandatoryQueryParam "to" Data.Time.Calendar.Day
      :> MandatoryQueryParam
           "earningType"
           Dashboard.Common.Driver.EarningType
      :> MandatoryQueryParam "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> Get
           '[JSON]
           EarningPeriodStatsRes
  )

type GetDriverEarningsHelper =
  ( "earnings" :> MandatoryQueryParam "from" Data.Time.Calendar.Day :> MandatoryQueryParam "to" Data.Time.Calendar.Day
      :> MandatoryQueryParam
           "earningType"
           Dashboard.Common.Driver.EarningType
      :> MandatoryQueryParam
           "driverId"
           (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> MandatoryQueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           EarningPeriodStatsRes
  )

type PostDriverUpdateMerchant =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updateMerchant" :> ReqBody '[JSON] UpdateDriverMerchantReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data DriverAPIs = DriverAPIs
  { getDriverDocumentsInfo :: EulerHS.Types.EulerClient Dashboard.Common.Driver.DriverDocumentsInfoRes,
    postDriverPersonNumbers :: (Data.ByteString.Lazy.ByteString, Dashboard.Common.PersonIdsReq) -> EulerHS.Types.EulerClient [Dashboard.Common.PersonRes],
    postDriverUpdateTagBulk :: (Data.ByteString.Lazy.ByteString, Dashboard.Common.UpdateTagBulkReq) -> EulerHS.Types.EulerClient [Dashboard.Common.UpdateTagBulkRes],
    postDriverDriverDataDecryption :: [DriverEncDataReq] -> EulerHS.Types.EulerClient [DriverDecDataResp],
    postDriverPersonId :: (Data.ByteString.Lazy.ByteString, Dashboard.Common.PersonMobileNoReq) -> EulerHS.Types.EulerClient [Dashboard.Common.PersonRes],
    getDriverAadhaarInfo :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient DriverAadhaarInfoRes,
    getDriverAadhaarInfobyMobileNumber :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient DriverAadhaarInfoByPhoneReq,
    getDriverList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient DriverListRes,
    getDriverActivity :: EulerHS.Types.EulerClient Dashboard.Common.Driver.DriverActivityRes,
    postDriverDisable :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverAcRestrictionUpdate :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UpdateACUsageRestrictionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverBlockWithReason :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> BlockDriverWithReasonReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverBlock :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverBlockReasonList :: EulerHS.Types.EulerClient [BlockReason],
    postDriverUnblock :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverLocation :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Dashboard.Common.Driver.DriverIds -> EulerHS.Types.EulerClient DriverLocationRes,
    deleteDriverPermanentlyDelete :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUnlinkDL :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUnlinkAadhaar :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdatePhoneNumber :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UpdatePhoneNumberReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateByPhoneNumber :: Kernel.Prelude.Text -> UpdateDriverDataReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateName :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UpdateDriverNameReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverDeleteRC :: Kernel.Types.Id.Id Dashboard.Common.Driver -> DeleteRCReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverClearStuckOnRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> EulerHS.Types.EulerClient ClearOnRideStuckDriversRes,
    postDriverSendDummyNotification :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverChangeOperatingCity :: Kernel.Types.Id.Id Dashboard.Common.Driver -> ChangeOperatingCityReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverGetOperatingCity :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> EulerHS.Types.EulerClient GetOperatingCityResp,
    postDriverPauseOrResumeServiceCharges :: Kernel.Types.Id.Id Dashboard.Common.Driver -> PauseOrResumeServiceChargesReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateRCInvalidStatus :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UpdateRCInvalidStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateRCInvalidStatusByRCNumber :: UpdateRCInvalidStatusByRCNumberReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateVehicleVariant :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UpdateVehicleVariantReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverBulkReviewRCVariant :: [ReviewRCVariantReq] -> EulerHS.Types.EulerClient [ReviewRCVariantRes],
    postDriverUpdateDriverTag :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UpdateDriverTagReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverClearFee :: Kernel.Types.Id.Id Dashboard.Common.Driver -> ClearDriverFeeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverPanAadharSelfieDetails :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient PanAadharSelfieDetailsResp,
    postDriverSyncDocAadharPan :: AadharPanSyncReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverUpdateVehicleManufacturing :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UpdateVehicleManufacturingReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverVehicleAppendSelectedServiceTiers :: Kernel.Types.Id.Id Dashboard.Common.Driver -> AppendSelectedServiceTiersReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverVehicleUpsertSelectedServiceTiers ::
      ( Data.ByteString.Lazy.ByteString,
        Dashboard.Common.Driver.UpsertDriverServiceTiersCsvReq
      ) ->
      EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverRefundByPayout :: Kernel.Types.Id.Id Dashboard.Common.Driver -> RefundByPayoutReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverSecurityDepositStatus :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Dashboard.Common.Driver.ServiceNames -> EulerHS.Types.EulerClient [SecurityDepositDfStatusRes],
    getDriverPanAadharSelfieDetailsList :: Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient [PanAadharSelfieDetailsListResp],
    postDriverBulkSubscriptionServiceUpdate :: BulkServiceUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverStats :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient DriverStatsRes,
    getDriverEarnings :: Data.Time.Calendar.Day -> Data.Time.Calendar.Day -> Dashboard.Common.Driver.EarningType -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient EarningPeriodStatsRes,
    postDriverUpdateMerchant :: Kernel.Types.Id.Id Dashboard.Common.Driver -> UpdateDriverMerchantReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    getDriverDocumentsInfo :<|> postDriverPersonNumbers :<|> postDriverUpdateTagBulk :<|> postDriverDriverDataDecryption :<|> postDriverPersonId :<|> getDriverAadhaarInfo :<|> getDriverAadhaarInfobyMobileNumber :<|> getDriverList :<|> getDriverActivity :<|> postDriverDisable :<|> postDriverAcRestrictionUpdate :<|> postDriverBlockWithReason :<|> postDriverBlock :<|> getDriverBlockReasonList :<|> postDriverUnblock :<|> getDriverLocation :<|> deleteDriverPermanentlyDelete :<|> postDriverUnlinkDL :<|> postDriverUnlinkAadhaar :<|> postDriverUpdatePhoneNumber :<|> postDriverUpdateByPhoneNumber :<|> postDriverUpdateName :<|> postDriverDeleteRC :<|> getDriverClearStuckOnRide :<|> postDriverSendDummyNotification :<|> postDriverChangeOperatingCity :<|> getDriverGetOperatingCity :<|> postDriverPauseOrResumeServiceCharges :<|> postDriverUpdateRCInvalidStatus :<|> postDriverUpdateRCInvalidStatusByRCNumber :<|> postDriverUpdateVehicleVariant :<|> postDriverBulkReviewRCVariant :<|> postDriverUpdateDriverTag :<|> postDriverClearFee :<|> getDriverPanAadharSelfieDetails :<|> postDriverSyncDocAadharPan :<|> postDriverUpdateVehicleManufacturing :<|> postDriverVehicleAppendSelectedServiceTiers :<|> postDriverVehicleUpsertSelectedServiceTiers :<|> postDriverRefundByPayout :<|> getDriverSecurityDepositStatus :<|> getDriverPanAadharSelfieDetailsList :<|> postDriverBulkSubscriptionServiceUpdate :<|> getDriverStats :<|> getDriverEarnings :<|> postDriverUpdateMerchant = driverClient

data DriverUserActionType
  = GET_DRIVER_DOCUMENTS_INFO
  | POST_DRIVER_PERSON_NUMBERS
  | POST_DRIVER_UPDATE_TAG_BULK
  | POST_DRIVER_DRIVER_DATA_DECRYPTION
  | POST_DRIVER_PERSON_ID
  | GET_DRIVER_AADHAAR_INFO
  | GET_DRIVER_AADHAAR_INFOBY_MOBILE_NUMBER
  | GET_DRIVER_LIST
  | GET_DRIVER_ACTIVITY
  | POST_DRIVER_DISABLE
  | POST_DRIVER_AC_RESTRICTION_UPDATE
  | POST_DRIVER_BLOCK_WITH_REASON
  | POST_DRIVER_BLOCK
  | GET_DRIVER_BLOCK_REASON_LIST
  | POST_DRIVER_UNBLOCK
  | GET_DRIVER_LOCATION
  | DELETE_DRIVER_PERMANENTLY_DELETE
  | POST_DRIVER_UNLINK_DL
  | POST_DRIVER_UNLINK_AADHAAR
  | POST_DRIVER_UPDATE_PHONE_NUMBER
  | POST_DRIVER_UPDATE_BY_PHONE_NUMBER
  | POST_DRIVER_UPDATE_NAME
  | POST_DRIVER_DELETE_RC
  | GET_DRIVER_CLEAR_STUCK_ON_RIDE
  | POST_DRIVER_SEND_DUMMY_NOTIFICATION
  | POST_DRIVER_CHANGE_OPERATING_CITY
  | GET_DRIVER_GET_OPERATING_CITY
  | POST_DRIVER_PAUSE_OR_RESUME_SERVICE_CHARGES
  | POST_DRIVER_UPDATE_RC_INVALID_STATUS
  | POST_DRIVER_UPDATE_RC_INVALID_STATUS_BY_RC_NUMBER
  | POST_DRIVER_UPDATE_VEHICLE_VARIANT
  | POST_DRIVER_BULK_REVIEW_RC_VARIANT
  | POST_DRIVER_UPDATE_DRIVER_TAG
  | POST_DRIVER_CLEAR_FEE
  | GET_DRIVER_PAN_AADHAR_SELFIE_DETAILS
  | POST_DRIVER_SYNC_DOC_AADHAR_PAN
  | POST_DRIVER_UPDATE_VEHICLE_MANUFACTURING
  | POST_DRIVER_VEHICLE_APPEND_SELECTED_SERVICE_TIERS
  | POST_DRIVER_VEHICLE_UPSERT_SELECTED_SERVICE_TIERS
  | POST_DRIVER_REFUND_BY_PAYOUT
  | GET_DRIVER_SECURITY_DEPOSIT_STATUS
  | GET_DRIVER_PAN_AADHAR_SELFIE_DETAILS_LIST
  | POST_DRIVER_BULK_SUBSCRIPTION_SERVICE_UPDATE
  | GET_DRIVER_STATS
  | GET_DRIVER_EARNINGS
  | POST_DRIVER_UPDATE_MERCHANT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''DriverUserActionType])
