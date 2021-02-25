module Types.Domain.FarePolicy where

import Beckn.Types.ID (ID)
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time (TimeOfDay)
import EulerHS.Prelude

data FarePolicy = FarePolicy
  { id :: ID FarePolicy,
    vehicleVariant :: Vehicle.Variant,
    organizationId :: ID Organization.Organization,
    baseFare :: Maybe Rational,
    baseDistance :: Maybe Rational,
    perExtraKmRate :: Rational,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Rational
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

defaultBaseFare :: Double
defaultBaseFare = 120.0

defaultBaseDistance :: Double
defaultBaseDistance = 5000.0

defaultPerExtraKmRate :: Double
defaultPerExtraKmRate = 12.0
