module Domain.Types.FarePolicy where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id (Id)
import qualified Domain.Types.Organization as Organization

data FarePolicy = FarePolicy
  { id :: Id FarePolicy,
    organizationId :: Id Organization.Organization,
    fareForPickup :: Amount,
    farePerKm :: Amount,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Amount,
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
  FarePolicyAPIEntity
    { fareForPickup = amountToDouble fareForPickup,
      farePerKm = amountToDouble farePerKm,
      nightShiftRate = amountToDouble <$> nightShiftRate,
      ..
    }
