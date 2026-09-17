-- | MSIL pilot: common override for quote.breakup[*].title. ONDC v2.1.0's
-- TRV10 schema restricts this field to a fixed eleven-value enum (BASE_FARE/
-- DISTANCE_FARE/CANCELLATION_CHARGES/REFUND/TOLL_CHARGES/PARKING_CHARGES/TAX/
-- WAITING_CHARGES/DRIVER_BATA/NIGHT_CHARGES/BUYER_ADDITIONAL_AMOUNT/ADD_ONS),
-- but the shared Layer 1 builder (Beckn.OnDemand.Utils.Common.mkQuotationBreakup,
-- used by on_init/on_confirm/on_select's DriverQuote-based path) emits
-- NammaYatri's much larger internal fare-component vocabulary instead --
-- valid for every merchant today but not ONDC-compliant. Most internal titles
-- either don't match at all, or match a different plural/spelling (e.g.
-- PARKING_CHARGE vs PARKING_CHARGES).
module Beckn.OnDemand.Utils.MSIL.Breakup
  ( remapBreakupTitle,
    overrideOrderBreakupTitles,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude

-- | Direct-named components remap 1:1; sub-components fold into their closest
-- allowed title (e.g. every toll-related line becomes TOLL_CHARGES, every
-- GST/government-rate line becomes TAX); anything with no reasonable target
-- (SERVICE_CHARGE, per-stop charges, luggage/airport/booth fees, ...) falls
-- into ADD_ONS; TOTAL_FARE has no target at all (Nothing) -- it's the sum,
-- not a component, and was never a valid breakup line to begin with.
remapBreakupTitle :: Text -> Maybe Text
remapBreakupTitle title
  | title == show Enums.TOTAL_FARE = Nothing
  | title == show Enums.BASE_FARE = Just "BASE_FARE"
  | title `elem` [show Enums.DISTANCE_FARE, show Enums.DEAD_KILOMETER_FARE, show Enums.TIME_BASED_FARE] = Just "DISTANCE_FARE"
  | title `elem` [show Enums.PARKING_CHARGE, show Enums.PARKING_CHARGE_TAX_EXCLUSIVE, show Enums.PARKING_CHARGE_TAX] = Just "PARKING_CHARGES"
  | title == show Enums.WAITING_OR_PICKUP_CHARGES = Just "WAITING_CHARGES"
  | title == show Enums.NIGHT_SHIFT_CHARGE = Just "NIGHT_CHARGES"
  | title `elem` [show Enums.TOLL_VAT, show Enums.TOLL_FARE_TAX_EXCLUSIVE, show Enums.TOLL_FARE_TAX, show Enums.TOLL_CHARGES] = Just "TOLL_CHARGES"
  | title `elem` [show Enums.CANCELLATION_CHARGES, show Enums.CANCELLATION_FEE_TAX_EXCLUSIVE, show Enums.CANCELLATION_TAX] = Just "CANCELLATION_CHARGES"
  | title == show Enums.CUSTOMER_SELECTED_FARE = Just "BUYER_ADDITIONAL_AMOUNT"
  | title == show Enums.DRIVER_ALLOWANCE = Just "DRIVER_BATA"
  | title `elem` [show Enums.SGST, show Enums.CGST, show Enums.FIXED_GOVERNMENT_RATE, show Enums.RIDE_VAT, show Enums.RIDE_FARE_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE, show Enums.RIDE_FARE_DISCOUNT_APPLICABLE_TAX, show Enums.RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX_EXCLUSIVE, show Enums.RIDE_FARE_NON_DISCOUNT_APPLICABLE_TAX] = Just "TAX"
  | title `elem` [show Enums.SERVICE_CHARGE, show Enums.EXTRA_TIME_FARE, show Enums.RIDE_STOP_CHARGES, show Enums.PER_STOP_CHARGES, show Enums.LUGGAGE_CHARGE, show Enums.AIRPORT_CONVENIENCE_FEE, show Enums.RETURN_FEE, show Enums.BOOTH_CHARGE, show Enums.PLATFORM_FEE, show Enums.DRIVER_SELECTED_FARE] = Just "ADD_ONS"
  | otherwise = Nothing

-- | Applies remapBreakupTitle to every breakup line on an already-built
-- order's quote -- for Layer 2 patches over Layer 1's mkQuotationBreakup
-- output (on_init, on_confirm). Lines with no valid target are dropped
-- rather than sent non-compliant.
overrideOrderBreakupTitles :: Spec.Order -> Spec.Order
overrideOrderBreakupTitles order = order {Spec.orderQuote = fixQuote <$> Spec.orderQuote order}
  where
    fixQuote quotation = quotation {Spec.quotationBreakup = mapMaybe fixBreakup <$> Spec.quotationBreakup quotation}
    fixBreakup breakup = case Spec.quotationBreakupInnerTitle breakup >>= remapBreakupTitle of
      Nothing -> Nothing
      Just newTitle -> Just breakup {Spec.quotationBreakupInnerTitle = Just newTitle}
