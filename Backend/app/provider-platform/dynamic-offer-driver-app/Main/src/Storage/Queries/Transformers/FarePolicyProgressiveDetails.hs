module Storage.Queries.Transformers.FarePolicyProgressiveDetails where

import Domain.Types.FarePolicy.Common (PickupCharges (..))
import Kernel.Prelude
import Kernel.Types.Common

mkPickupCharges :: Maybe Money -> Maybe Money -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Money -> Maybe HighPrecMoney -> PickupCharges
mkPickupCharges pickupChargesMin pickupChargesMax pickupChargesMinAmount pickupChargesMaxAmount deadKmFare deadKmFareAmount =
  let deadKmFareHP = mkAmountWithDefault deadKmFareAmount deadKmFare
   in PickupCharges
        { pickupChargesMin = mkAmountWithDefault pickupChargesMinAmount (fromMaybe (roundToIntegral deadKmFareHP) pickupChargesMin),
          pickupChargesMax = mkAmountWithDefault pickupChargesMaxAmount (fromMaybe (roundToIntegral deadKmFareHP) pickupChargesMax)
        }
