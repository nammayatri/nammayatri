module Product.FareCalculator.Models.FarePolicy where

import Beckn.Types.ID
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time
import EulerHS.Prelude

data FarePolicy = FarePolicy
  { id :: ID FarePolicy,
    vehicleType :: Vehicle.Variant,
    organizationId :: ID Organization.Organization,
    baseFare :: Maybe Rational,
    baseDistance :: Maybe Rational,
    perExtraKmRate :: Rational,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay,
    nightShiftRate :: Rational
  }
  deriving (Generic, Show, Eq)
