{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.PassService where

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

data PassAPIEntity = PassAPIEntity {category :: PassCategoryAPIEntity, entityType :: PassTypeAPIEntity, info :: [PassInfoAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassCategoryAPIEntity = PassCategoryAPIEntity {description :: Data.Text.Text, id :: Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory, name :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassInfoAPIEntity = PassInfoAPIEntity
  { amount :: Kernel.Types.Common.HighPrecMoney,
    benefit :: Data.Maybe.Maybe Domain.Types.Pass.Benefit,
    days :: Data.Maybe.Maybe Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.Pass.Pass,
    savings :: Data.Maybe.Maybe Kernel.Types.Common.HighPrecMoney,
    trips :: Data.Maybe.Maybe Kernel.Prelude.Int,
    vehicleServiceTierType :: BecknV2.FRFS.Enums.ServiceTierType
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassSelectionAPIEntity = PassSelectionAPIEntity
  { paymentOrder :: Data.Maybe.Maybe Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp,
    purchasedPassId :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PassTypeAPIEntity = PassTypeAPIEntity
  { catchline :: Data.Maybe.Maybe Data.Text.Text,
    description :: Data.Maybe.Maybe Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PassType.PassType,
    name :: Data.Maybe.Maybe Data.Text.Text,
    title :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PurchasedPassAPIEntity = PurchasedPassAPIEntity
  { expiryDate :: Data.Maybe.Maybe Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass,
    passEntity :: PassAPIEntity,
    purchaseDate :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.PurchasedPass.StatusType,
    tripsLeft :: Data.Maybe.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
