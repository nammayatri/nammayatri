{-# LANGUAGE DerivingVia #-}

module Types.API.FarePolicy.Rentals where

import Beckn.Prelude
import Beckn.Types.Predicate
import Beckn.Utils.Validation (Validate, validateField)
import Domain.Types.RentalFarePolicy (RentalFarePolicyAPIEntity)
import qualified Domain.Types.Vehicle as Vehicle

newtype ListRentalFarePoliciesRes = ListRentalFarePoliciesRes
  { rentalFarePolicies :: [RentalFarePolicyAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype CreateRentalFarePolicyReq = CreateRentalFarePolicyReq
  { createList :: NonEmpty CreateRentalFarePolicyItem
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateRentalFarePolicyItem = CreateRentalFarePolicyItem
  { vehicleVariant :: Vehicle.Variant,
    baseFare :: Double,
    baseDistance :: Double, -- Distance
    baseDurationHr :: Int,
    extraKmFare :: Double,
    extraMinuteFare :: Double,
    driverAllowanceForDay :: Maybe Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

validateCreateRentalsFarePolicyRequest :: Validate CreateRentalFarePolicyItem
validateCreateRentalsFarePolicyRequest CreateRentalFarePolicyItem {..} =
  sequenceA_
    [ validateField "baseFare" baseFare $ Min @Double 0,
      validateField "baseDistance" baseDistance $ Min @Double 0,
      validateField "baseDurationHr" baseDurationHr $ Min @Int 0,
      validateField "extraKmFare" extraKmFare $ Min @Double 0,
      validateField "extraMinuteFare" extraMinuteFare $ Min @Double 0,
      validateField "driverAllowanceForDay" driverAllowanceForDay $ InMaybe $ Min @Double 0
    ]
