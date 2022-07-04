module Domain.Types.FarePolicy where

import Beckn.Prelude
import Beckn.Types.Id (Id)
import qualified Domain.Types.Organization as Organization

data FarePolicy = FarePolicy
  { id :: Id FarePolicy,
    organizationId :: Id Organization.Organization,
    fareForPickup :: Double,
    farePerKm :: Double,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

data FarePolicyAPIEntity = FarePolicyAPIEntity
  { id :: Id FarePolicy,
    fareForPickup :: Double,
    farePerKm :: Double,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeFarePolicyAPIEntity :: FarePolicy -> FarePolicyAPIEntity
makeFarePolicyAPIEntity FarePolicy {..} =
  FarePolicyAPIEntity {..}
