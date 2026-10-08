{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Pure end-ride toll reconciliation, extracted verbatim from the inline
-- matrix that lived in Domain.Action.UI.Ride.EndRide so it can be unit-tested
-- as a truth table. Behavior-identical to the extracted code.
--
-- Truth table (gpsUnreliable = distance calc failed OR any self-tuned batch;
-- pending = validated pending toll, only applicable when pickup/drop within
-- threshold):
--
-- gpsUnreliable, estimated toll present:
--   estimated == 0                          -> bill nothing, no confidence
--   detected present, pending applicable    -> detected + pending, Neutral
--   detected present, otherwise             -> detected,           Neutral
--   none detected, deviated-to-toll-route   -> estimated,          Neutral
--   none detected, pending applicable       -> detected + pending, Neutral
--   none detected, nothing pending:
--     estimated-fallback enabled & in-threshold -> estimated,      Unsure
--     otherwise                                 -> detected (none), Unsure
-- gpsUnreliable, no estimated toll:
--   pending applicable                      -> pending,            Unsure
--   otherwise                               -> detected,           no confidence
-- gps reliable:
--   detected, no pending                    -> detected,           Sure
--   detected + pending applicable           -> detected + pending, Neutral
--   none detected + pending applicable      -> pending,            Neutral
--   otherwise, estimated > 0                -> detected,           Sure
--   otherwise                               -> detected,           no confidence
--
-- Known limitation carried over (documented follow-up in
-- docs/backend/design/fare-recompute-unification-plan.md): on the
-- approx-route / downward recompute branches these inputs come from possibly
-- stale detection; tolls are not re-derived from the billed route.
module Domain.Action.UI.Ride.EndRide.TollDecision
  ( TollInput (..),
    TollBilling (..),
    decideTollBilling,
  )
where

import EulerHS.Prelude
import Kernel.Types.Common
import Kernel.Types.Confidence

data TollInput = TollInput
  { distanceCalculationFailed :: Bool,
    numberOfSelfTuned :: Maybe Int,
    pickupDropOutsideOfThreshold :: Bool,
    estimatedTollCharges :: Maybe HighPrecMoney,
    estimatedTollNames :: Maybe [Text],
    estimatedTollIds :: Maybe [Text],
    detectedTollCharges :: Maybe HighPrecMoney,
    detectedTollNames :: Maybe [Text],
    detectedTollIds :: Maybe [Text],
    driverDeviatedToTollRoute :: Maybe Bool,
    validatedPendingToll :: Maybe (HighPrecMoney, [Text], [Text]),
    enableEstimatedTollFallback :: Bool
  }
  deriving (Show, Generic)

data TollBilling = TollBilling
  { tollCharges :: Maybe HighPrecMoney,
    tollNames :: Maybe [Text],
    tollIds :: Maybe [Text],
    tollConfidence :: Maybe Confidence
  }
  deriving (Show, Eq, Generic)

decideTollBilling :: TollInput -> TollBilling
decideTollBilling TollInput {..} = do
  let gpsUnreliable = distanceCalculationFailed || maybe False (> 0) numberOfSelfTuned
      -- Only apply validated pending toll if pickup/drop is within threshold (route was as expected)
      canApplyValidatedPendingToll = not pickupDropOutsideOfThreshold
      detected = TollBilling detectedTollCharges detectedTollNames detectedTollIds
      combinedWithPending (pendingCharges, pendingNames, pendingIds) =
        TollBilling
          { tollCharges = Just (fromMaybe 0 detectedTollCharges + pendingCharges),
            tollNames = Just (fromMaybe [] detectedTollNames <> pendingNames),
            tollIds = Just (fromMaybe [] detectedTollIds <> pendingIds),
            tollConfidence = Just Neutral
          }
  if gpsUnreliable
    then
      if isJust estimatedTollCharges
        then
          if estimatedTollCharges == Just 0
            then TollBilling Nothing Nothing Nothing Nothing
            else
              if isJust detectedTollCharges
                then case (canApplyValidatedPendingToll, validatedPendingToll) of
                  (True, Just pending) ->
                    -- Some detected + some pending (same as distance calc success case)
                    combinedWithPending pending
                  _ ->
                    -- No pending tolls or route deviated
                    detected (Just Neutral)
                else
                  if driverDeviatedToTollRoute == Just True
                    then TollBilling estimatedTollCharges estimatedTollNames estimatedTollIds (Just Neutral)
                    else case (canApplyValidatedPendingToll, validatedPendingToll) of
                      (True, Just pending) ->
                        -- Combine detected + pending tolls
                        combinedWithPending pending
                      _ ->
                        -- Nothing detected and nothing pending: GPS was dark around the gates, so
                        -- neither the billing walk nor the deviation walk has any signal
                        if enableEstimatedTollFallback && canApplyValidatedPendingToll
                          then TollBilling estimatedTollCharges estimatedTollNames estimatedTollIds (Just Unsure)
                          else detected (Just Unsure)
        else case (canApplyValidatedPendingToll, validatedPendingToll) of
          (True, Just (pendingCharges, pendingNames, pendingIds)) ->
            TollBilling (Just pendingCharges) (Just pendingNames) (Just pendingIds) (Just Unsure)
          _ -> detected Nothing
    else case (detectedTollCharges, canApplyValidatedPendingToll, validatedPendingToll) of
      (Just charges, _, Nothing) ->
        TollBilling (Just charges) detectedTollNames detectedTollIds (Just Sure)
      (Just _, True, Just pending) ->
        -- Some detected + some pending
        combinedWithPending pending
      (Nothing, True, Just (pendingCharges, pendingNames, pendingIds)) ->
        TollBilling (Just pendingCharges) (Just pendingNames) (Just pendingIds) (Just Neutral)
      _ ->
        if maybe False (> 0) estimatedTollCharges
          then detected (Just Sure)
          else detected Nothing
