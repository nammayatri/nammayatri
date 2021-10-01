module Types.Domain.FarePolicy where

import Beckn.Types.Id (Id)
import Data.Time (TimeOfDay)
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Vehicle as Vehicle

data ExtraKmRate = ExtraKmRate
  { fromExtraDistance :: Rational,
    extraFare :: Rational
  }
  deriving (Generic, Show, Eq)

data FarePolicy = FarePolicy
  { id :: Id FarePolicy,
    vehicleVariant :: Vehicle.Variant,
    organizationId :: Id Organization.Organization,
    baseFare :: Maybe Rational,
    baseDistance :: Maybe Rational,
    perExtraKmRateList :: [ExtraKmRate],
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Rational
  }
  deriving (Generic, Show, Eq)

defaultBaseFare :: Double
defaultBaseFare = 120.0

defaultBaseDistance :: Double
defaultBaseDistance = 5000.0

defaultExtraKmRate :: ExtraKmRate
defaultExtraKmRate = ExtraKmRate 0 12.0

data ExtraKmRateAPIEntity = ExtraKmRateAPIEntity
  { fromExtraDistance :: Double,
    extraFare :: Double
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

makeExtraKmRateAPIEntity :: ExtraKmRate -> ExtraKmRateAPIEntity
makeExtraKmRateAPIEntity ExtraKmRate {..} =
  ExtraKmRateAPIEntity
    { fromExtraDistance = fromRational fromExtraDistance,
      extraFare = fromRational extraFare
    }

fromExtraKmRateAPIEntity :: ExtraKmRateAPIEntity -> ExtraKmRate
fromExtraKmRateAPIEntity ExtraKmRateAPIEntity {..} =
  ExtraKmRate
    { fromExtraDistance = toRational fromExtraDistance,
      extraFare = toRational extraFare
    }