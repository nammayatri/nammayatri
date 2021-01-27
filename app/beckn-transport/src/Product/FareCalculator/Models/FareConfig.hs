module Product.FareCalculator.Models.FareConfig where

import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import EulerHS.Prelude
import Product.FareCalculator.Models.ID

data FareConfig = FareConfig
  { id :: ID FareConfig,
    vehicleType :: Vehicle.Variant,
    organizationId :: ID Organization.Organization,
    minimumFare :: Maybe Rational,
    perKmRate :: Rational,
    minimumDistance :: Maybe Float,
    multiplicationFactor :: Maybe Int
  }
  deriving (Generic, Show, Eq)
