{-# LANGUAGE TypeApplications #-}

module Types.API.FarePolicy
  ( ListFarePolicyResponse (..),
    UpdateFarePolicyRequest (..),
    FarePolicyResponse (..),
    UpdateFarePolicyResponse,
    validateUpdateFarePolicyRequest,
  )
where

import Beckn.Types.APISuccess
import Beckn.Types.Id (Id)
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Data.Time (TimeOfDay (..))
import EulerHS.Prelude hiding (id)
import Types.Domain.FarePolicy (FarePolicy)
import qualified Types.Storage.Vehicle as Vehicle

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
  deriving (Generic, Show, FromJSON)

type UpdateFarePolicyResponse = APISuccess

validateUpdateFarePolicyRequest :: Validate UpdateFarePolicyRequest
validateUpdateFarePolicyRequest UpdateFarePolicyRequest {..} =
  sequenceA_
    [ validateField "baseFare" baseFare . InMaybe $ InRange @Double 0 500,
      validateField "baseDistance" baseDistance . InMaybe $ InRange @Double 0 10000,
      validateField "perExtraKmRate" perExtraKmRate $ InRange @Double 1 99,
      validateField "nightShiftRate" nightShiftRate . InMaybe $ InRange @Double 1 2,
      validateField "nightShiftStart" nightShiftStart . InMaybe $ InRange (TimeOfDay 18 0 0) (TimeOfDay 23 30 0),
      validateField "nightShiftEnd" nightShiftEnd . InMaybe $ InRange (TimeOfDay 0 30 0) (TimeOfDay 7 0 0)
    ]
