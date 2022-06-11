module Domain.Types.FarePolicy where

import Beckn.Prelude
import Beckn.Types.Id (Id)
import Domain.Types.FarePolicy.PerExtraKmRate
import qualified Domain.Types.Organization as Organization

data FarePolicy = FarePolicy
  { id :: Id FarePolicy,
    organizationId :: Id Organization.Organization,
    baseFare :: Maybe Rational,
    perExtraKmRateList :: NonEmpty PerExtraKmRate,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Rational,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)

data FarePolicyAPIEntity = FarePolicyAPIEntity
  { id :: Id FarePolicy,
    baseFare :: Maybe Double,
    perExtraKmRateList :: NonEmpty PerExtraKmRateAPIEntity,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeFarePolicyAPIEntity :: FarePolicy -> FarePolicyAPIEntity
makeFarePolicyAPIEntity FarePolicy {..} =
  FarePolicyAPIEntity
    { id = id,
      baseFare = fromRational <$> baseFare,
      perExtraKmRateList = makePerExtraKmRateAPIEntity <$> perExtraKmRateList,
      nightShiftStart = nightShiftStart,
      nightShiftEnd = nightShiftEnd,
      nightShiftRate = fromRational <$> nightShiftRate,
      ..
    }
