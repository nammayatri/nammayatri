{-
 Copyright 2022-25, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

 Module      : SharedLogic.FareCalculatorV2
 Version     : 1.0.0
 Author      : Namma Yatri Development Team
 Description : Enhanced fare calculation with configurable charges (VAT, commission, toll tax)
               Extends the base fare calculation with configurable percentage or fixed charges
               that can be applied to specific fare components via fare_policy configuration.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | FareCalculatorV2: Enhanced fare calculation with configurable charges
--
-- This module provides calculateFareParametersV2, which extends the base fare calculation
-- with configurable VAT, commission, and toll tax charges. Charges can be configured
-- via fare_policy table with percentage or fixed values, and can target specific
-- fare components (e.g., ride fare, waiting charge, toll charges).
--
-- Key features:
-- - Configurable VAT charges (e.g., 14% on ride fare + congestion charge)
-- - Configurable commission (e.g., 8% on ride fare base)
-- - Configurable toll VAT (e.g., 25% on toll charges)
-- - Component-based charge application (can target specific fare components)
-- - Backward compatible (extends v1 calculation, doesn't replace it)
module SharedLogic.FareCalculatorV2
  ( calculateFareParametersV2,
    calculateCommission,
  )
where

import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Domain.Types.FareParameters
import qualified Domain.Types.FareParameters as DFParams
import Domain.Types.FarePolicy
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.FareCalculator as FC
import qualified Storage.Cac.TransporterConfig as SCTC

-- | Map of fare components to their monetary values
-- Used to compute charges on specific components (e.g., VAT on RideFare + CongestionChargeComponent)
type ComponentMap = Map.Map FareChargeComponent HighPrecMoney

-- | Parsed charge value - either a percentage or fixed amount
data ParsedCodeValue
  = ParsedPercentage Rational -- e.g., "14%" -> 0.14
  | ParsedFixed HighPrecMoney -- e.g., "50" -> 50.0

-- | Calculate fare parameters with configurable charges (VAT, toll tax)
--
-- This function:
-- 1. Calls the base v1 fare calculation to get standard fare components
-- 2. Checks if enableFareCalculatorV2 is enabled in TransporterConfig
-- 3. If enabled, applies configurable charges (VAT, toll tax) based on fare_policy configuration
-- 4. If disabled, returns the base fare parameters (fallback to v1 behavior)
-- 5. Returns FareParameters with new breakdown fields (rideVat, tollVat)
--
-- Note: Commission is NOT stored in FareParameters. It's calculated separately using
-- calculateCommission and stored in Booking/Ride tables.
--
-- The new charges are stored separately in fare_parameters table for transparency,
-- and are included in pureFareSum for final fare calculation.
--
-- Example: If fare_policy has vat_charge_config = {"value":"14%","appliesOn":["RideFare","DeadKmFareComponent"]},
-- then VAT will be calculated as 14% of (RideFare + DeadKmFareComponent)
calculateFareParametersV2 ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  FC.CalculateFareParametersParams ->
  m FareParameters
calculateFareParametersV2 params = do
  -- First, calculate base fare using v1 calculator
  baseFareParams <- FC.calculateFareParameters params
  -- Check if V2 features are enabled via TransporterConfig
  isV2Enabled <- case params.merchantOperatingCityId of
    Just merchantOpCityId -> do
      transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing
      let v2Enabled = maybe False (fromMaybe False . (.enableFareCalculatorV2)) transporterConfig
      logDebug $ "FareCalculatorV2: TransporterConfig for merchantOpCityId " <> merchantOpCityId.getId <> " - enableFareCalculatorV2: " <> show (transporterConfig >>= (.enableFareCalculatorV2)) <> ", V2 enabled: " <> show v2Enabled
      pure v2Enabled
    Nothing -> do
      logDebug "FareCalculatorV2: No merchantOperatingCityId provided, using v1 behavior"
      pure False -- If no merchantOperatingCityId, default to v1 behavior
      -- Apply configurable charges only if V2 is enabled
  if isV2Enabled
    then applyConfiguredCharges params.farePolicy baseFareParams
    else do
      logDebug "FareCalculatorV2: V2 disabled or not enabled, using v1 behavior (no configurable charges applied)"
      pure baseFareParams

-- | Apply configurable charges (VAT, commission, toll tax) to fare parameters
--
-- Builds a component map from fare parameters and computes charges based on
-- fare_policy configuration. Charges are only calculated if explicitly configured.
--
-- Note: Commission is calculated and stored for breakdown/transparency but is NOT
-- added to the final fare sum (as per PRD requirements).
applyConfiguredCharges :: MonadFlow m => FullFarePolicy -> FareParameters -> m FareParameters
applyConfiguredCharges farePolicy fareParams = do
  -- Build a map of all fare components (base + details) for charge calculation
  let componentMap = buildComponentMap fareParams

  -- Calculate VAT: Only if configured in fare_policy.vat_charge_config
  -- Example config: {"value":"14%","appliesOn":["RideFare","CongestionChargeComponent"]}
  rideVatValue <- case farePolicy.vatChargeConfig of
    Just config -> do
      vatAmount <- computeConfiguredCharge "vatCharge" componentMap (Just config)
      pure $ if vatAmount > 0 then Just vatAmount else Nothing
    Nothing -> pure Nothing -- No VAT if not configured

  -- Calculate toll VAT: Only if configured in fare_policy.toll_tax_charge_config
  -- Example config: {"value":"25%","appliesOn":["TollChargesComponent"]}
  tollVatValue <- case farePolicy.tollTaxChargeConfig of
    Just config -> do
      tollVatAmount <- computeConfiguredCharge "tollTaxCharge" componentMap (Just config)
      pure $ if tollVatAmount > 0 then Just tollVatAmount else Nothing
    Nothing -> pure Nothing -- No toll VAT if not configured

  -- Return updated fare parameters with new breakdown fields
  -- Note: Commission is NOT stored in FareParameters - it's calculated separately via calculateCommission and stored in Booking/Ride tables
  pure $
    fareParams
      { paymentProcessingFee = Nothing, -- TODO: Will be enhanced when payment context is available
        rideVat = rideVatValue,
        tollVat = tollVatValue
      }

-- | Compute a configured charge (VAT, commission, or toll tax)
--
-- Steps:
-- 1. Sum up the monetary values of all components specified in config.appliesOn
-- 2. Parse the charge value (percentage like "14%" or fixed like "50")
-- 3. Apply the charge to the base amount
--
-- Example: If config = {"value":"14%","appliesOn":["RideFare","DeadKmFareComponent"]}
-- and RideFare=100, DeadKmFareComponent=20, then:
-- - baseAmount = 120
-- - result = 120 * 0.14 = 16.8
computeConfiguredCharge ::
  MonadFlow m =>
  Text -> -- Label for logging (e.g., "vatCharge", "commissionCharge")
  ComponentMap -> -- Map of all fare components to their values
  Maybe FareChargeConfig -> -- Charge configuration from fare_policy
  m HighPrecMoney
computeConfiguredCharge label componentMap = \case
  Nothing -> pure 0
  Just FareChargeConfig {..} -> do
    -- If no components specified, default to RideFare
    let baseComponents = if null appliesOn then [RideFare] else appliesOn
        -- Sum up all component amounts to get the base for charge calculation
        baseAmount = sum $ fmap (componentAmount componentMap) baseComponents
    case parseCodeValue value of
      Nothing -> do
        logWarning $ "Unable to parse charge value for " <> label <> " with raw value: " <> value
        pure 0
      Just parsedValue ->
        -- Apply percentage or fixed value to base amount
        pure $ applyParsedValue baseAmount parsedValue

-- | Parse a charge value from Text (percentage like "14%" or fixed like "50")
-- Returns Nothing if the value cannot be parsed
parseCodeValue :: Text -> Maybe ParsedCodeValue
parseCodeValue rawValue =
  let trimmed = T.strip rawValue
   in if T.null trimmed
        then Nothing
        else
          if "%" `T.isSuffixOf` trimmed
            then -- Percentage value: "14%" -> ParsedPercentage 0.14

              let numeric = T.stripEnd $ T.dropEnd 1 trimmed
               in ParsedPercentage <$> parsePercentage numeric
            else -- Fixed value: "50" -> ParsedFixed 50.0
              ParsedFixed . HighPrecMoney . toRational <$> parseFixed trimmed

-- | Parse a percentage string and convert to Rational (e.g., "14" -> 0.14)
parsePercentage :: Text -> Maybe Rational
parsePercentage txt = (/ 100) . toRational <$> parseFixed txt

-- | Parse a fixed numeric string to Double (e.g., "50" -> 50.0)
-- Handles decimals and negative numbers
parseFixed :: Text -> Maybe Double
parseFixed txt =
  let numericOnly = T.filter (\c -> isDigit c || c == '.' || c == '-') txt
   in readMaybe (T.unpack numericOnly)

-- | Apply a parsed charge value to a base amount
-- If percentage: multiply base by percentage (e.g., 100 * 0.14 = 14)
-- If fixed: return the fixed amount directly
applyParsedValue :: HighPrecMoney -> ParsedCodeValue -> HighPrecMoney
applyParsedValue baseAmount = \case
  ParsedPercentage pct -> HighPrecMoney (baseAmount.getHighPrecMoney * pct)
  ParsedFixed amount -> amount

-- | Build a map of all fare components from FareParameters
--
-- This map includes:
-- - Base fare components (RideFare, WaitingCharge, ServiceCharge, etc.)
-- - Detail-specific components based on fare type (Progressive, Rental, InterCity, Ambulance)
--
-- The map is used to compute charges on specific components as configured in fare_policy.
-- Example: If VAT config specifies appliesOn: ["RideFare", "DeadKmFareComponent"],
-- this map allows us to look up those components and sum them for VAT calculation.
buildComponentMap :: FareParameters -> ComponentMap
buildComponentMap FareParameters {..} =
  let maybeZero = fromMaybe 0
      -- Base map: Always includes these common fare components
      baseMap =
        Map.fromList
          [ (RideFare, baseFare),
            (WaitingCharge, maybeZero waitingCharge),
            (ServiceChargeComponent, maybeZero serviceCharge),
            (TollChargesComponent, maybeZero tollCharges),
            (CongestionChargeComponent, maybeZero congestionCharge),
            (ParkingChargeComponent, maybeZero parkingCharge),
            (PetChargeComponent, maybeZero petCharges),
            (PriorityChargeComponent, maybeZero priorityCharges),
            (NightShiftChargeComponent, maybeZero nightShiftCharge),
            (InsuranceChargeComponent, maybeZero insuranceCharge),
            (StopChargeComponent, maybeZero stopCharges),
            (CustomerCancellationChargeComponent, maybeZero customerCancellationDues),
            (PlatformFeeComponent, maybeZero platformFee),
            (RideVatComponent, maybeZero rideVat),
            (TollVatComponent, maybeZero tollVat),
            (StateEntryPermitChargesComponent, maybeZero stateEntryPermitCharges)
          ]
      -- Detail map: Additional components based on fare policy type
      detailMap = case fareParametersDetails of
        DFParams.ProgressiveDetails det ->
          Map.fromList
            [ (DeadKmFareComponent, det.deadKmFare),
              (ExtraKmFareComponent, maybeZero det.extraKmFare),
              (RideDurationFareComponent, maybeZero det.rideDurationFare)
            ]
        DFParams.RentalDetails det ->
          Map.fromList
            [ (TimeBasedFareComponent, det.timeBasedFare),
              (DistBasedFareComponent, det.distBasedFare),
              (DeadKmFareComponent, det.deadKmFare)
            ]
        DFParams.InterCityDetails det ->
          Map.fromList
            [ (TimeFareComponent, det.timeFare),
              (DistanceFareComponent, det.distanceFare),
              (PickupChargeComponent, det.pickupCharge),
              (ExtraDistanceFareComponent, det.extraDistanceFare),
              (ExtraTimeFareComponent, det.extraTimeFare)
            ]
        DFParams.AmbulanceDetails det ->
          Map.fromList
            [ (AmbulanceDistBasedFareComponent, det.distBasedFare)
            ]
        DFParams.SlabDetails _ ->
          Map.empty -- Slab details don't have additional components
          -- Merge base and detail maps (detail map takes precedence if key exists in both)
   in Map.union baseMap detailMap

-- | Get the monetary value of a component from the component map
-- Returns 0 if the component is not found
componentAmount :: ComponentMap -> FareChargeComponent -> HighPrecMoney
componentAmount mp key = Map.findWithDefault 0 key mp

-- | Calculate commission separately (not part of fare)
--
-- This function calculates commission based on fare_policy.commission_charge_config
-- and FareParameters. Commission is NOT included in the fare sum - it's stored
-- separately in booking and ride tables for transparency.
--
-- Example: If fare_policy has commission_charge_config = {"value":"8%","appliesOn":["RideFare"]},
-- then commission will be calculated as 8% of RideFare.
--
-- Returns Nothing if commission is not configured or if amount is 0.
calculateCommission ::
  MonadFlow m =>
  FareParameters ->
  Maybe FullFarePolicy ->
  m (Maybe HighPrecMoney)
calculateCommission fareParams mbFarePolicy = do
  case mbFarePolicy of
    Nothing -> pure Nothing
    Just farePolicy -> do
      let componentMap = buildComponentMap fareParams
      case farePolicy.commissionChargeConfig of
        Just config -> do
          commAmount <- computeConfiguredCharge "commissionCharge" componentMap (Just config)
          pure $ if commAmount > 0 then Just commAmount else Nothing
        Nothing -> pure Nothing
