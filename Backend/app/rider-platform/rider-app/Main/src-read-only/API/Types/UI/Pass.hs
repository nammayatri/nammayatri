{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Pass where

import qualified BecknV2.FRFS.Enums
import qualified Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Time
import qualified Domain.Types.Pass
import qualified Domain.Types.PassCategory
import qualified Domain.Types.PassType
import qualified Domain.Types.PurchasedPass
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import qualified SharedLogic.Offer
import Tools.Auth

data PassAPIEntity = PassAPIEntity
  { amount :: Kernel.Types.Common.HighPrecMoney,
    autoApply :: Kernel.Prelude.Bool,
    benefit :: Data.Maybe.Maybe Domain.Types.Pass.Benefit,
    benefitDescription :: Data.Text.Text,
    code :: Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    documentsRequired :: [Domain.Types.Pass.PassDocumentType],
    eligibility :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.Pass.Pass,
    maxDays :: Data.Maybe.Maybe Kernel.Prelude.Int,
    maxTrips :: Data.Maybe.Maybe Kernel.Prelude.Int,
    name :: Data.Maybe.Maybe Data.Text.Text,
    offer :: Data.Maybe.Maybe SharedLogic.Offer.CumulativeOfferResp,
    originalAmount :: Kernel.Types.Common.HighPrecMoney,
    savings :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney,
    vehicleServiceTierType :: [BecknV2.FRFS.Enums.ServiceTierType]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassCategoryAPIEntity = PassCategoryAPIEntity {description :: Data.Text.Text, id :: Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory, name :: Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassDetailsAPIEntity = PassDetailsAPIEntity {category :: PassCategoryAPIEntity, passDetails :: PassAPIEntity, passType :: PassTypeAPIEntity}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassInfoAPIEntity = PassInfoAPIEntity {passCategory :: PassCategoryAPIEntity, passTypes :: [PassTypeAPIEntity], passes :: [PassAPIEntity]}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassSelectReq = PassSelectReq {imeiNumber :: Data.Text.Text, profilePicture :: Data.Text.Text, startDate :: Data.Time.Day}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassSelectionAPIEntity = PassSelectionAPIEntity
  { paymentOrder :: Data.Maybe.Maybe Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp,
    purchasedPassId :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassSwitchDeviceIdReq = PassSwitchDeviceIdReq {deviceId :: Data.Maybe.Maybe Data.Text.Text, imeiNumber :: Data.Maybe.Maybe Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassTypeAPIEntity = PassTypeAPIEntity
  { catchline :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PassType.PassType,
    name :: Data.Maybe.Maybe Data.Text.Text,
    passEnum :: Data.Maybe.Maybe Domain.Types.PassType.PassEnum,
    title :: Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassUploadProfilePictureReq = PassUploadProfilePictureReq {imeiNumber :: Data.Text.Text, profilePicture :: Data.Text.Text, purchasedPassId :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassVerifyReq = PassVerifyReq
  { autoActivated :: Data.Maybe.Maybe Kernel.Prelude.Bool,
    currentLat :: Data.Maybe.Maybe Kernel.Prelude.Double,
    currentLon :: Data.Maybe.Maybe Kernel.Prelude.Double,
    stopId :: Data.Maybe.Maybe Data.Text.Text,
    vehicleNumber :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PurchasedPassAPIEntity = PurchasedPassAPIEntity
  { daysToExpire :: Kernel.Prelude.Int,
    deviceMismatch :: Kernel.Prelude.Bool,
    deviceSwitchAllowed :: Kernel.Prelude.Bool,
    expiryDate :: Data.Time.Day,
    futureRenewals :: [PurchasedPassTransactionAPIEntity],
    id :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass,
    isAutoVerified :: Kernel.Prelude.Bool,
    isPreferredSourceAndDestinationSet :: Kernel.Prelude.Bool,
    lastVerifiedVehicleNumber :: Data.Maybe.Maybe Data.Text.Text,
    passEntity :: PassDetailsAPIEntity,
    passNumber :: Data.Text.Text,
    profilePicture :: Data.Maybe.Maybe Data.Text.Text,
    purchaseDate :: Data.Time.Day,
    startDate :: Data.Time.Day,
    status :: Domain.Types.PurchasedPass.StatusType,
    tripsLeft :: Data.Maybe.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PurchasedPassTransactionAPIEntity = PurchasedPassTransactionAPIEntity
  { amount :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    endDate :: Data.Time.Day,
    passCode :: Data.Text.Text,
    passName :: Data.Maybe.Maybe Data.Text.Text,
    passType :: Data.Maybe.Maybe Domain.Types.PassType.PassEnum,
    startDate :: Data.Time.Day,
    status :: Domain.Types.PurchasedPass.StatusType
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SetPassPrefSrcAndDestReq = SetPassPrefSrcAndDestReq {prefDest :: Data.Text.Text, prefSrc :: Data.Text.Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
