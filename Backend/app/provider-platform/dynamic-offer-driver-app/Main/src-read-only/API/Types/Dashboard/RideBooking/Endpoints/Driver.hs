{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Driver where

import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.Common
import qualified Domain.Types.Ride
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Lib.Yudhishthira.Types
import Servant
import Servant.Client

data DriverBlockTransactions = DriverBlockTransactions
  { reasonCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blockReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blockTimeInHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    reportedAt :: Kernel.Prelude.UTCTime,
    blockLiftTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    blockedBy :: Kernel.Prelude.Text,
    requestorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blockReasonFlag :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverFeedbackAPIEntity = DriverFeedbackAPIEntity
  { rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    createdAt :: Kernel.Prelude.UTCTime,
    feedbackText :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    feedbackDetails :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverInfoRes = DriverInfoRes
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    firstName :: Kernel.Prelude.Text,
    middleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    numberOfRides :: Kernel.Prelude.Int,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    blocked :: Kernel.Prelude.Bool,
    blockedReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verified :: Kernel.Prelude.Bool,
    subscribed :: Kernel.Prelude.Bool,
    canDowngradeToSedan :: Kernel.Prelude.Bool,
    canDowngradeToHatchback :: Kernel.Prelude.Bool,
    canDowngradeToTaxi :: Kernel.Prelude.Bool,
    canSwitchToRental :: Kernel.Prelude.Bool,
    canSwitchToInterCity :: Kernel.Prelude.Bool,
    canSwitchToIntraCity :: Kernel.Prelude.Bool,
    vehicleNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    selectedServiceTiers :: [Kernel.Prelude.Text],
    driverLicenseDetails :: Kernel.Prelude.Maybe DriverLicenseAPIEntity,
    vehicleRegistrationDetails :: [DriverRCAssociationAPIEntity],
    onboardingDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    lastActivityDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    drunkAndDriveViolationCount :: Kernel.Prelude.Int,
    bundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    reactVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    alternateNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    availableMerchants :: [Kernel.Prelude.Text],
    merchantOperatingCity :: Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City,
    currentAcOffReportCount :: Kernel.Prelude.Int,
    totalAcRestrictionUnblockCount :: Kernel.Prelude.Int,
    lastACStatusCheckedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    currentACStatus :: Kernel.Prelude.Bool,
    downgradeReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    assignedCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    cancelledCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    cancellationRate :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    windowSize :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    blockedDueToRiderComplains :: Kernel.Prelude.Bool,
    blockStateModifier :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverTag :: Kernel.Prelude.Maybe [Lib.Yudhishthira.Types.TagNameValue],
    driverTagObject :: Kernel.Prelude.Maybe [Lib.Yudhishthira.Types.TagObject],
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blockedInfo :: [DriverBlockTransactions],
    softBlockStiers :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    softBlockExpiryTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    softBlockReasonFlag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverMode :: Kernel.Prelude.Maybe Domain.Types.Common.DriverMode,
    lastOfflineTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverLicenseAPIEntity = DriverLicenseAPIEntity
  { driverLicenseId :: Kernel.Types.Id.Id Dashboard.Common.Driver.DriverLicense,
    documentImageId1 :: Kernel.Types.Id.Id Dashboard.Common.Image,
    documentImageId2 :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Image),
    driverDob :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    licenseNumber :: Kernel.Prelude.Text,
    licenseExpiry :: Kernel.Prelude.UTCTime,
    classOfVehicles :: [Kernel.Prelude.Text],
    failedRules :: [Kernel.Prelude.Text],
    verificationStatus :: Dashboard.Common.VerificationStatus,
    consent :: Kernel.Prelude.Bool,
    consentTimestamp :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverOutstandingBalanceResp = DriverOutstandingBalanceResp
  { driverFeeId :: Kernel.Types.Id.Id DriverOutstandingBalanceResp,
    driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    govtCharges :: Kernel.Types.Common.Money,
    govtChargesWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    platformFee :: PlatformFee,
    numRides :: Kernel.Prelude.Int,
    payBy :: Kernel.Prelude.UTCTime,
    totalFee :: Kernel.Types.Common.Money,
    totalEarnings :: Kernel.Types.Common.Money,
    totalFeeWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    totalEarningsWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    startTime :: Kernel.Prelude.UTCTime,
    endTime :: Kernel.Prelude.UTCTime,
    status :: Dashboard.Common.Driver.DriverFeeStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverRCAssociationAPIEntity = DriverRCAssociationAPIEntity
  { associatedOn :: Kernel.Prelude.UTCTime,
    associatedTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    isRcActive :: Kernel.Prelude.Bool,
    details :: VehicleRegistrationCertificateAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ExemptionAndCashCollectionDriverFeeReq = ExemptionAndCashCollectionDriverFeeReq {paymentIds :: [Kernel.Prelude.Text], isExempt :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ExemptionAndCashCollectionDriverFeeReq where
  hideSecrets = Kernel.Prelude.identity

data GetFeedbackListRes = GetFeedbackListRes {feedbacks :: [DriverFeedbackAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PlatformFee = PlatformFee
  { fee :: Kernel.Types.Common.HighPrecMoney,
    cgst :: Kernel.Types.Common.HighPrecMoney,
    sgst :: Kernel.Types.Common.HighPrecMoney,
    feeWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    cgstWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    sgstWithCurrency :: Kernel.Types.Common.PriceAPIEntity
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleRegistrationCertificateAPIEntity = VehicleRegistrationCertificateAPIEntity
  { registrationCertificateId :: Kernel.Types.Id.Id Dashboard.Common.Driver.VehicleRegistrationCertificate,
    documentImageId :: Kernel.Types.Id.Id Dashboard.Common.Image,
    certificateNumber :: Kernel.Prelude.Text,
    fitnessExpiry :: Kernel.Prelude.UTCTime,
    permitExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    pucExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    insuranceValidity :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    vehicleClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    failedRules :: [Kernel.Prelude.Text],
    vehicleManufacturer :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleEnergyType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reviewRequired :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    reviewedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    manufacturerModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    verificationStatus :: Dashboard.Common.VerificationStatus,
    fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("driver" :> (GetDriverPaymentDue :<|> PostDriverEnable :<|> PostDriverCollectCashHelper :<|> PostDriverCollectCashV2Helper :<|> PostDriverExemptCashHelper :<|> PostDriverExemptCashV2Helper :<|> GetDriverInfoHelper :<|> GetDriverFeedbackList :<|> PostDriverUnlinkVehicle :<|> PostDriverEndRCAssociation :<|> PostDriverAddVehicle :<|> PostDriverSetRCStatus :<|> PostDriverExemptDriverFeeV2Helper))

type GetDriverPaymentDue = ("paymentDue" :> QueryParam "countryCode" Kernel.Prelude.Text :> MandatoryQueryParam "phone" Kernel.Prelude.Text :> Get '[JSON] [DriverOutstandingBalanceResp])

type PostDriverEnable = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "enable" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverCollectCash = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "collectCash" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverCollectCashHelper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "collectCash" :> ReqBody '[JSON] Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverV2CollectCash =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "v2" :> "collectCash" :> Capture "serviceName" Dashboard.Common.Driver.ServiceNames
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverCollectCashV2Helper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "collectCash" :> "v2" :> Capture "token" Kernel.Prelude.Text
      :> Capture
           "serviceName"
           Dashboard.Common.Driver.ServiceNames
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverExemptCash = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "exemptCash" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverExemptCashHelper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "exemptCash" :> ReqBody '[JSON] Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverV2ExemptCash =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "v2" :> "exemptCash" :> Capture "serviceName" Dashboard.Common.Driver.ServiceNames
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverExemptCashV2Helper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "exemptCash" :> "v2" :> Capture "token" Kernel.Prelude.Text
      :> Capture
           "serviceName"
           Dashboard.Common.Driver.ServiceNames
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetDriverInfo =
  ( "info" :> QueryParam "mobileNumber" Kernel.Prelude.Text :> QueryParam "mobileCountryCode" Kernel.Prelude.Text
      :> QueryParam
           "vehicleNumber"
           Kernel.Prelude.Text
      :> QueryParam "dlNumber" Kernel.Prelude.Text
      :> QueryParam "rcNumber" Kernel.Prelude.Text
      :> QueryParam
           "email"
           Kernel.Prelude.Text
      :> QueryParam
           "personId"
           (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> Get
           '[JSON]
           DriverInfoRes
  )

type GetDriverInfoHelper =
  ( "info" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> Capture "mbFleet" Kernel.Prelude.Bool
      :> QueryParam
           "mobileNumber"
           Kernel.Prelude.Text
      :> QueryParam "mobileCountryCode" Kernel.Prelude.Text
      :> QueryParam "vehicleNumber" Kernel.Prelude.Text
      :> QueryParam
           "dlNumber"
           Kernel.Prelude.Text
      :> QueryParam
           "rcNumber"
           Kernel.Prelude.Text
      :> QueryParam
           "email"
           Kernel.Prelude.Text
      :> QueryParam
           "personId"
           (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> Get
           '[JSON]
           DriverInfoRes
  )

type GetDriverFeedbackList =
  ( "feedback" :> "list" :> QueryParam "personId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> QueryParam
           "mobileNumber"
           Kernel.Prelude.Text
      :> QueryParam "mobileCountryCode" Kernel.Prelude.Text
      :> Get '[JSON] GetFeedbackListRes
  )

type PostDriverUnlinkVehicle = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "unlinkVehicle" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverEndRCAssociation = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "endRCAssociation" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostDriverAddVehicle =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "addVehicle"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverSetRCStatus =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "setRCStatus"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverExemptDriverFee =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "exemptDriverFee"
      :> Capture
           "serviceName"
           Dashboard.Common.Driver.ServiceNames
      :> ReqBody '[JSON] ExemptionAndCashCollectionDriverFeeReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverExemptDriverFeeV2Helper =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "exemptDriverFee" :> "v2" :> Capture "token" Kernel.Prelude.Text
      :> Capture
           "serviceName"
           Dashboard.Common.Driver.ServiceNames
      :> ReqBody '[JSON] ExemptionAndCashCollectionDriverFeeReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data DriverAPIs = DriverAPIs
  { getDriverPaymentDue :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient [DriverOutstandingBalanceResp],
    postDriverEnable :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverCollectCash :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverV2CollectCash :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Dashboard.Common.Driver.ServiceNames -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverExemptCash :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverV2ExemptCash :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Dashboard.Common.Driver.ServiceNames -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverInfo :: Kernel.Prelude.Text -> Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> EulerHS.Types.EulerClient DriverInfoRes,
    getDriverFeedbackList :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient GetFeedbackListRes,
    postDriverUnlinkVehicle :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverEndRCAssociation :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverAddVehicle :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.AddVehicleReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverSetRCStatus :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Fleet.Driver.RCStatusReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverExemptDriverFee :: Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Text -> Dashboard.Common.Driver.ServiceNames -> ExemptionAndCashCollectionDriverFeeReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    getDriverPaymentDue :<|> postDriverEnable :<|> postDriverCollectCash :<|> postDriverV2CollectCash :<|> postDriverExemptCash :<|> postDriverV2ExemptCash :<|> getDriverInfo :<|> getDriverFeedbackList :<|> postDriverUnlinkVehicle :<|> postDriverEndRCAssociation :<|> postDriverAddVehicle :<|> postDriverSetRCStatus :<|> postDriverExemptDriverFee = driverClient

data DriverUserActionType
  = GET_DRIVER_PAYMENT_DUE
  | POST_DRIVER_ENABLE
  | POST_DRIVER_COLLECT_CASH
  | POST_DRIVER_V2_COLLECT_CASH
  | POST_DRIVER_EXEMPT_CASH
  | POST_DRIVER_V2_EXEMPT_CASH
  | GET_DRIVER_INFO
  | GET_DRIVER_FEEDBACK_LIST
  | POST_DRIVER_UNLINK_VEHICLE
  | POST_DRIVER_END_RC_ASSOCIATION
  | POST_DRIVER_ADD_VEHICLE
  | POST_DRIVER_SET_RC_STATUS
  | POST_DRIVER_EXEMPT_DRIVER_FEE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''DriverUserActionType])
