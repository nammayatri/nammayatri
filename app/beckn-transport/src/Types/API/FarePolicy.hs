{-# LANGUAGE TypeApplications #-}

module Types.API.FarePolicy
  ( ListFarePolicyResponse (..),
    UpdateFarePolicyRequest (..),
    FarePolicyResponse (..),
    UpdateFarePolicyResponse,
  )
where

import Beckn.Types.APISuccess
import Beckn.Types.Id (Id)
import Beckn.Types.Predicate
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import Beckn.Utils.Validation
import Data.Time (TimeOfDay (..))
import EulerHS.Prelude hiding (id)
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
  deriving (Generic, Show)

type UpdateFarePolicyResponse = APISuccess

instance FromJSON UpdateFarePolicyRequest where
  parseJSON = genericParseJsonWithValidation "UpdateFarePolicyRequest" validateUpdateFarePolicyRequest

validateUpdateFarePolicyRequest :: Validate UpdateFarePolicyRequest
validateUpdateFarePolicyRequest UpdateFarePolicyRequest {..} =
  sequenceA_
    [ validateMaybe "baseFare" baseFare $ InRange @Double 0 1000,
      validateMaybe "baseDistance" baseDistance $ InRange @Double 0 10000,
      validateMaybe "nightShiftRate" nightShiftRate $ InRange @Double 1 2,
      validateMaybe "nightShiftStart" nightShiftStart $ InRange (TimeOfDay 18 0 0) (TimeOfDay 23 0 0),
      validateMaybe "nightShiftEnd" nightShiftEnd $ InRange (TimeOfDay 7 0 0) (TimeOfDay 12 30 0)
    ]
