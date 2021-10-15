{-# LANGUAGE TypeApplications #-}

module Types.Domain.FarePolicy.Discount where

import Beckn.Types.Id (Id)
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Data.Time (TimeOfDay, UTCTime)
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle
import Data.OpenApi (ToSchema)

data Discount = Discount
  { id :: Id Discount,
    vehicleVariant :: Vehicle.Variant,
    organizationId :: Id Organization.Organization,
    startTime :: TimeOfDay,
    endTime :: TimeOfDay,
    discount :: Rational,
    enabled :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

data DiscountAPIEntity = DiscountAPIEntity
  { id :: Id Discount,
    startTime :: TimeOfDay,
    endTime :: TimeOfDay,
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