{-# LANGUAGE TypeApplications #-}

module Types.Domain.FarePolicy where

import Beckn.Types.Id (Id)
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Data.Time (TimeOfDay)
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle

data PerExtraKmRate = PerExtraKmRate
  { distanceRangeStart :: Rational,
    fare :: Rational
  }
  deriving (Generic, Show, Eq)

data FarePolicy = FarePolicy
  { id :: Id FarePolicy,
    vehicleVariant :: Vehicle.Variant,
    organizationId :: Id Organization.Organization,
    baseFare :: Maybe Rational,
    perExtraKmRateList :: NonEmpty PerExtraKmRate,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Rational
  }
  deriving (Generic, Show, Eq)

data PerExtraKmRateAPIEntity = PerExtraKmRateAPIEntity
  { distanceRangeStart :: Double,
    fare :: Double
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

makeExtraKmRateAPIEntity :: PerExtraKmRate -> PerExtraKmRateAPIEntity
makeExtraKmRateAPIEntity PerExtraKmRate {..} =
  PerExtraKmRateAPIEntity
    { distanceRangeStart = fromRational distanceRangeStart,
      fare = fromRational fare
    }

fromExtraKmRateAPIEntity :: PerExtraKmRateAPIEntity -> PerExtraKmRate
fromExtraKmRateAPIEntity PerExtraKmRateAPIEntity {..} =
  PerExtraKmRate
    { distanceRangeStart = toRational distanceRangeStart,
      fare = toRational fare
    }

validatePerExtraKmRateAPIEntity :: Validate PerExtraKmRateAPIEntity
validatePerExtraKmRateAPIEntity extraKmRate =
  sequenceA_
    [ validateField "fare" extraKmRate.fare $ InRange @Double 1 99,
      validateField "distanceRangeStart" extraKmRate.distanceRangeStart $ Min @Double 0
    ]
