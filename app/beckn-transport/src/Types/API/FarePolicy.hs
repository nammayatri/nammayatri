{-# LANGUAGE TypeApplications #-}

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
import EulerHS.Prelude hiding (id)
import Types.Domain.FarePolicy (FarePolicyAPIEntity, ExtraKmRateAPIEntity)

newtype ListFarePolicyRes = ListFarePolicyRes
  { farePolicies :: [FarePolicyAPIEntity]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data UpdateFarePolicyReq = UpdateFarePolicyReq
  { baseFare :: Maybe Double,
    baseDistance :: Maybe Double,
    perExtraKmRateList :: [ExtraKmRateAPIEntity],
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateFarePolicyRes = APISuccess

validateUpdateFarePolicyRequest :: Validate UpdateFarePolicyReq
validateUpdateFarePolicyRequest UpdateFarePolicyReq {..} =
  sequenceA_
    [ validateField "baseFare" baseFare . InMaybe $ InRange @Double 0 500,
      validateField "baseDistance" baseDistance . InMaybe $ InRange @Double 0 10000,
      traverse_ (\extraKmRate -> validateObject "perExtraKmRateList" extraKmRate validatePerExtraKmRate) perExtraKmRateList,
      validateField "nightShiftRate" nightShiftRate . InMaybe $ InRange @Double 1 2,
      validateField "nightShiftStart" nightShiftStart . InMaybe $ InRange (TimeOfDay 18 0 0) (TimeOfDay 23 30 0),
      validateField "nightShiftEnd" nightShiftEnd . InMaybe $ InRange (TimeOfDay 0 30 0) (TimeOfDay 7 0 0)
    ]
  where
    validatePerExtraKmRate extraKmRate =
      sequenceA_
        [ validateField "extraFare" extraKmRate.extraFare $ InRange @Double 1 99,
          validateField "fromExtraDistance" extraKmRate.fromExtraDistance $ Min @Double 0
        ]
