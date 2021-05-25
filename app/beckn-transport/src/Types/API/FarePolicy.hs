module Types.API.FarePolicy
  ( ListFarePolicyResponse (..),
    UpdateFarePolicyRequest (..),
    FarePolicyResponse (..),
    UpdateFarePolicyResponse,
  )
where

import Beckn.Types.APISuccess
import Beckn.Types.Id (Id)
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Data.Time (TimeOfDay)
import EulerHS.Prelude
import Types.Domain.FarePolicy (FarePolicy)

data FarePolicyResponse = FarePolicyResponse
  { id :: Id FarePolicy,
    vehicleVariant :: Vehicle.Variant,
    baseFare :: Maybe Double,
    baseDistance :: Maybe Double,
    perExtraKmRate :: Double,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype ListFarePolicyResponse = ListFarePolicyResponse
  { farePolicies :: [FarePolicyResponse]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data UpdateFarePolicyRequest = UpdateFarePolicyRequest
  { baseFare :: Maybe Double,
    baseDistance :: Maybe Double,
    perExtraKmRate :: Double,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type UpdateFarePolicyResponse = APISuccess
