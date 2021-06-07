module Types.Domain.FarePolicy where

import Beckn.Types.Id (Id)
import qualified Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time (TimeOfDay)
import EulerHS.Prelude hiding (id)

data FarePolicy = FarePolicy
  { id :: Id FarePolicy,
    vehicleVariant :: Vehicle.Variant,
    organizationId :: Id Organization.Organization,
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
