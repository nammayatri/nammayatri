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
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Vehicle as Veh

data CreateFarePolicyDiscountReq = CreateFarePolicyDiscountReq
  { vehicleVariant :: Veh.Variant,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Double,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type CreateFarePolicyDiscountRes = APISuccess

validateCreateFarePolicyDiscountReq :: Validate CreateFarePolicyDiscountReq
validateCreateFarePolicyDiscountReq CreateFarePolicyDiscountReq {..} =
  validateField "discount" discount $ Min @Double 0.01

data UpdateFarePolicyDiscountReq = UpdateFarePolicyDiscountReq
  { fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Double,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateFarePolicyDiscountRes = APISuccess

validateUpdateFarePolicyDiscountReq :: Validate UpdateFarePolicyDiscountReq
validateUpdateFarePolicyDiscountReq UpdateFarePolicyDiscountReq {..} =
  validateField "discount" discount $ Min @Double 0.01

type DeleteFarePolicyDiscountRes = APISuccess
