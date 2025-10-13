{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Pass where

import qualified BecknV2.FRFS.Enums
import qualified Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Text
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
import Tools.Auth

data PassAPIEntity = PassAPIEntity
  { amount :: Kernel.Types.Common.HighPrecMoney,
    autoApply :: Kernel.Prelude.Bool,
    benefit :: Data.Maybe.Maybe Domain.Types.Pass.Benefit,
    benefitDescription :: Data.Text.Text,
    code :: Data.Text.Text,
    documentsRequired :: [Domain.Types.Pass.PassDocumentType],
    eligibility :: Kernel.Prelude.Bool,
    id :: Kernel.Types.Id.Id Domain.Types.Pass.Pass,
    maxDays :: Data.Maybe.Maybe Kernel.Prelude.Int,
    maxTrips :: Data.Maybe.Maybe Kernel.Prelude.Int,
    name :: Data.Maybe.Maybe Data.Text.Text,
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

data PassSelectionAPIEntity = PassSelectionAPIEntity
  { paymentOrder :: Data.Maybe.Maybe Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp,
    purchasedPassId :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassTypeAPIEntity = PassTypeAPIEntity
  { catchline :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PassType.PassType,
    name :: Data.Maybe.Maybe Data.Text.Text,
    title :: Data.Text.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PurchasedPassAPIEntity = PurchasedPassAPIEntity
  { expiryDate :: Data.Maybe.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass,
    passEntity :: PassDetailsAPIEntity,
    purchaseDate :: Kernel.Prelude.UTCTime,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.PurchasedPass.PurchasedPass,
    status :: Domain.Types.PurchasedPass.StatusType,
    tripsLeft :: Data.Maybe.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
