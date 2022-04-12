{-# LANGUAGE TypeApplications #-}

module Domain.Types.FarePolicy.Discount where

import Beckn.Types.Id (Id)
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import qualified Domain.Types.FarePolicy.FareProduct as DFareProduct
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import Storage.Tabular.FarePolicy.FareProduct ()

data Discount = Discount
  { id :: Id Discount,
    vehicleVariant :: DVeh.Variant,
    organizationId :: Id DOrg.Organization,
    fareProductType :: DFareProduct.FareProductType,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Rational,
    enabled :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

data DiscountAPIEntity = DiscountAPIEntity
  { id :: Id Discount,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Double,
    enabled :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

makeDiscountAPIEntity :: Discount -> DiscountAPIEntity
makeDiscountAPIEntity Discount {..} =
  DiscountAPIEntity
    { discount = fromRational discount,
      ..
    }

validateDiscountAPIEntity :: Validate DiscountAPIEntity
validateDiscountAPIEntity discountApiEntity =
  validateField "discount" discountApiEntity.discount $ Min @Double 0.01
