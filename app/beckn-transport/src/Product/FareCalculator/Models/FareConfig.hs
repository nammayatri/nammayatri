module Product.FareCalculator.Models.FareConfig where

import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time
import EulerHS.Prelude
import Product.FareCalculator.Models.ID

data FareConfig = FareConfig
  { id :: ID FareConfig,
    vehicleType :: Vehicle.Variant,
    organizationId :: ID Organization.Organization,
    baseFare :: Maybe Rational,
    baseDistance :: Maybe Rational,
    perExtraKmRate :: Rational,
    perDeadKmRate :: Rational,
    minDeadKmThreshold :: Maybe Rational,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay,
    nightShiftRate :: Rational
  }
  deriving (Generic, Show, Eq)
