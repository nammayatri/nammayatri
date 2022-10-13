module Domain.Types.FarePolicy.Discount where

import Beckn.Types.Common (Money)
import Beckn.Types.Id (Id)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Domain.Types.Common
import qualified Domain.Types.FarePolicy.FareProduct as DFareProduct
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)

data DiscountD s = Discount
  { id :: Id Discount,
    vehicleVariant :: DVeh.Variant,
    organizationId :: Id DOrg.Organization,
    fareProductType :: DFareProduct.FareProductType,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Money,
    enabled :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

type Discount = DiscountD 'Safe

instance FromJSON (DiscountD 'Unsafe)

instance ToJSON (DiscountD 'Unsafe)

data DiscountAPIEntity = DiscountAPIEntity
  { id :: Id Discount,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Money,
    enabled :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makeDiscountAPIEntity :: Discount -> DiscountAPIEntity
makeDiscountAPIEntity Discount {..} = DiscountAPIEntity {..}
