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
  { extraDistanceRangeStart :: Rational,
    extraFare :: Rational
  }
  deriving (Generic, Show, Eq)

data FarePolicy = FarePolicy
  { id :: Id FarePolicy,
    vehicleVariant :: Vehicle.Variant,
    organizationId :: Id Organization.Organization,
    baseFare :: Maybe Rational,
    baseDistance :: Maybe Rational,
    perExtraKmRateList :: [PerExtraKmRate],
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Rational
  }
  deriving (Generic, Show, Eq)

defaultBaseFare :: Double
defaultBaseFare = 120.0

defaultBaseDistance :: Double
defaultBaseDistance = 5000.0

defaultPerExtraKmRate :: PerExtraKmRate
defaultPerExtraKmRate = PerExtraKmRate 0 12.0

data PerExtraKmRateAPIEntity = PerExtraKmRateAPIEntity
  { extraDistanceRangeStart :: Double,
    extraFare :: Double
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

makeExtraKmRateAPIEntity :: PerExtraKmRate -> PerExtraKmRateAPIEntity
makeExtraKmRateAPIEntity PerExtraKmRate {..} =
  PerExtraKmRateAPIEntity
    { extraDistanceRangeStart = fromRational extraDistanceRangeStart,
      extraFare = fromRational extraFare
    }

fromExtraKmRateAPIEntity :: PerExtraKmRateAPIEntity -> PerExtraKmRate
fromExtraKmRateAPIEntity PerExtraKmRateAPIEntity {..} =
  PerExtraKmRate
    { extraDistanceRangeStart = toRational extraDistanceRangeStart,
      extraFare = toRational extraFare
    }

validatePerExtraKmRateAPIEntity :: Validate PerExtraKmRateAPIEntity
validatePerExtraKmRateAPIEntity extraKmRate =
  sequenceA_
    [ validateField "extraFare" extraKmRate.extraFare $ InRange @Double 1 99,
      validateField "extraDistanceRangeStart" extraKmRate.extraDistanceRangeStart $ Min @Double 0
    ]
