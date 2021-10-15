{-# LANGUAGE TypeApplications #-}

module Types.API.FarePolicy.Discount
  ( UpdateFarePolicyDiscountReq,
    UpdateFarePolicyDiscountRes,
    CreateFarePolicyDiscountReq,
    CreateFarePolicyDiscountRes,
    DeleteFarePolicyDiscountRes,
    validateCreateFarePolicyDiscountReq,
    validateUpdateFarePolicyDiscountReq,
  )
where

import Beckn.Types.APISuccess
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Data.Time (TimeOfDay (..))
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Vehicle as Veh

data CreateFarePolicyDiscountReq = CreateFarePolicyDiscountReq
  { vehicleVariant :: Veh.Variant,
    startTime :: TimeOfDay,
    endTime :: TimeOfDay,
    discount :: Double,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON)

type CreateFarePolicyDiscountRes = APISuccess

validateCreateFarePolicyDiscountReq :: Validate CreateFarePolicyDiscountReq
validateCreateFarePolicyDiscountReq CreateFarePolicyDiscountReq {..} =
  validateField "discount" discount $ Min @Double 0.01

data UpdateFarePolicyDiscountReq = UpdateFarePolicyDiscountReq
  { startTime :: TimeOfDay,
    endTime :: TimeOfDay,
    discount :: Double,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON)

type UpdateFarePolicyDiscountRes = APISuccess

validateUpdateFarePolicyDiscountReq :: Validate UpdateFarePolicyDiscountReq
validateUpdateFarePolicyDiscountReq UpdateFarePolicyDiscountReq {..} =
  validateField "discount" discount $ Min @Double 0.01

type DeleteFarePolicyDiscountRes = APISuccess
