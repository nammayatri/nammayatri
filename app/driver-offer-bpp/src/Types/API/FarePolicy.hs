module Types.API.FarePolicy
  ( ListFarePolicyRes (..),
    UpdateFarePolicyReq (..),
    UpdateFarePolicyRes,
    validateUpdateFarePolicyRequest,
  )
where

import Beckn.Types.APISuccess
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import Data.Time (TimeOfDay (..))
import Domain.Types.FarePolicy
import EulerHS.Prelude hiding (id)
import Utils.Common

newtype ListFarePolicyRes = ListFarePolicyRes
  { oneWayFarePolicies :: [FarePolicyAPIEntity]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data UpdateFarePolicyReq = UpdateFarePolicyReq
  { baseDistancePerKmFare :: HighPrecMoney,
    baseDistance :: Meters,
    perExtraKmFare :: HighPrecMoney,
    deadKmFare :: Money, -- constant value
    driverExtraFeeList :: [Money],
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateFarePolicyRes = APISuccess

validateUpdateFarePolicyRequest :: Validate UpdateFarePolicyReq
validateUpdateFarePolicyRequest UpdateFarePolicyReq {..} =
  sequenceA_ --FIXME: ask for lower and upper bounds for all the values
    [ validateField "baseDistancePerKmFare" baseDistancePerKmFare $ InRange @HighPrecMoney 0 500,
      validateField "baseDistance" baseDistance $ InRange @Meters 0 500,
      validateField "perExtraKmFare" perExtraKmFare $ InRange @HighPrecMoney 0 500,
      validateField "deadKmFare" deadKmFare $ InRange @Money 0 500,
      validateList "driverExtraFeeList" driverExtraFeeList $ \x -> validateField "fee" x $ InRange @Money 0 500,
      validateField "nightShiftRate" nightShiftRate . InMaybe $ InRange @Double 1 2,
      validateField "nightShiftStart" nightShiftStart . InMaybe $ InRange (TimeOfDay 18 0 0) (TimeOfDay 23 30 0),
      validateField "nightShiftEnd" nightShiftEnd . InMaybe $ InRange (TimeOfDay 0 30 0) (TimeOfDay 7 0 0)
    ]
