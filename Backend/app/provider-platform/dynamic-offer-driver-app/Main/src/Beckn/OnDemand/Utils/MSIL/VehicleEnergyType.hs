-- | MSIL pilot: common override for the wire vehicle.energy_type code
-- (BecknV2.OnDemand.Types.vehicleEnergyType, sourced from vehicle.energyType
-- -- free text captured at driver onboarding, not validated against ONDC's
-- vocabulary), shared across every MSIL transformer that sends
-- order.fulfillments[].vehicle to the BAP (OnSelect/OnInit/OnConfirm/
-- OnStatus, ...).
--
-- ONDC v2.1.0 only accepts seven energy_type codes (ELECTRIC, PETROL, DIESEL,
-- HYDROGEN, BIOFUELS, CNG, LPG -- BecknV2.OnDemand.Enums.EnergyType). For
-- MSIL, whatever value Layer 1 put there is checked against that vocabulary;
-- anything outside it (including onboarding free text that doesn't match) is
-- overridden to PETROL. A missing value (Nothing -- we simply aren't sending
-- one) is left as Nothing.
module Beckn.OnDemand.Utils.MSIL.VehicleEnergyType
  ( patchOrderVehicleEnergyType,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import Kernel.Prelude

validEnergyTypes :: [Text]
validEnergyTypes = show <$> [Enums.ELECTRIC, Enums.PETROL, Enums.DIESEL, Enums.HYDROGEN, Enums.BIOFUELS, Enums.CNG, Enums.LPG]

-- | Any of the seven ONDC v2.1.0 energy_type codes passes through unchanged;
-- anything else (including free-text onboarding values that don't match)
-- becomes PETROL.
overrideVehicleEnergyType :: Text -> Text
overrideVehicleEnergyType energyType
  | energyType `elem` validEnergyTypes = energyType
  | otherwise = show Enums.PETROL -- Need to discuss what should be passed if it is not c

-- | The single patch operation for an order: overrides every one of the
-- order's fulfillments' vehicle.energy_type per the rule above. Every other
-- field is passed through untouched.
patchOrderVehicleEnergyType :: Spec.Order -> Spec.Order
patchOrderVehicleEnergyType order =
  order {Spec.orderFulfillments = map patchFulfillment <$> order.orderFulfillments}
  where
    patchFulfillment fulfillment = fulfillment {Spec.fulfillmentVehicle = patchVehicle <$> fulfillment.fulfillmentVehicle}
    patchVehicle vehicle = vehicle {Spec.vehicleEnergyType = overrideVehicleEnergyType <$> vehicle.vehicleEnergyType}
