{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.TollChargeDecision
  ( TollChargeDecisionInput (..),
    TollChargeDecision (..),
    resolveTollChargeAndConfidence,
  )
where

import EulerHS.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Confidence

data TollChargeDecisionInput = TollChargeDecisionInput
  { distanceCalculationFailed :: Bool,
    numberOfSelfTuned :: Maybe Int,
    detectedTollCharges :: Maybe HighPrecMoney,
    detectedTollNames :: Maybe [Text],
    detectedTollIds :: Maybe [Text],
    estimatedTollCharges :: Maybe HighPrecMoney,
    estimatedTollNames :: Maybe [Text],
    estimatedTollIds :: Maybe [Text],
    driverDeviatedToTollRoute :: Maybe Bool,
    pickupDropOutsideOfThreshold :: Bool,
    validatedPendingToll :: Maybe (HighPrecMoney, [Text], [Text]),
    enableEstimatedTollFallback :: Bool
  }

-- | hasNoTollEvidence is true when a toll was estimated but the ride has no detected toll, no matched
-- pending toll and no toll-route deviation flag, so the outcome rests on the estimate alone.
data TollChargeDecision = TollChargeDecision
  { tollCharges :: Maybe HighPrecMoney,
    tollNames :: Maybe [Text],
    tollIds :: Maybe [Text],
    tollConfidence :: Maybe Confidence,
    hasNoTollEvidence :: Bool
  }

resolveTollChargeAndConfidence :: TollChargeDecisionInput -> TollChargeDecision
resolveTollChargeAndConfidence TollChargeDecisionInput {..} =
  TollChargeDecision {tollCharges, tollNames, tollIds, tollConfidence, hasNoTollEvidence}
  where
    pendingTollMatched = not pickupDropOutsideOfThreshold && isJust validatedPendingToll
    rawGpsSawToll = (distanceCalculationFailed || maybe False (> 0) numberOfSelfTuned) && driverDeviatedToTollRoute == Just True
    hasNoTollEvidence = maybe False (> 0) estimatedTollCharges && isNothing detectedTollCharges && not rawGpsSawToll && not pendingTollMatched
    (tollCharges, tollNames, tollIds, tollConfidence) = do
      let distanceCalculationFailure = distanceCalculationFailed || (maybe False (> 0) numberOfSelfTuned)
          -- Only apply validated pending toll if pickup/drop is within threshold (route was as expected)
          canApplyValidatedPendingToll = not pickupDropOutsideOfThreshold
      if distanceCalculationFailure
        then
          if isJust estimatedTollCharges
            then
              if estimatedTollCharges == Just 0
                then (Nothing, Nothing, Nothing, Nothing)
                else
                  if isJust detectedTollCharges
                    then case (canApplyValidatedPendingToll, validatedPendingToll) of
                      (True, Just (pendingCharges, pendingNames, pendingIds)) ->
                        -- Some detected + some pending (same as distance calc success case)
                        let combinedCharges = fromMaybe 0 detectedTollCharges + pendingCharges
                            combinedNames = fromMaybe [] detectedTollNames <> pendingNames
                            combinedIds = fromMaybe [] detectedTollIds <> pendingIds
                         in (Just combinedCharges, Just combinedNames, Just combinedIds, Just Neutral)
                      _ ->
                        -- No pending tolls or route deviated
                        (detectedTollCharges, detectedTollNames, detectedTollIds, Just Neutral)
                    else
                      if driverDeviatedToTollRoute == Just True
                        then (estimatedTollCharges, estimatedTollNames, estimatedTollIds, Just Neutral)
                        else case (canApplyValidatedPendingToll, validatedPendingToll) of
                          (True, Just (pendingCharges, pendingNames, pendingIds)) ->
                            -- Combine detected + pending tolls
                            let combinedCharges = fromMaybe 0 detectedTollCharges + pendingCharges
                                combinedNames = fromMaybe [] detectedTollNames <> pendingNames
                                combinedIds = fromMaybe [] detectedTollIds <> pendingIds
                             in (Just combinedCharges, Just combinedNames, Just combinedIds, Just Neutral)
                          _ ->
                            -- Nothing detected and nothing pending: GPS was dark around the gates, so
                            -- neither the billing walk nor the deviation walk has any signal
                            if enableEstimatedTollFallback && canApplyValidatedPendingToll
                              then (estimatedTollCharges, estimatedTollNames, estimatedTollIds, Just Unsure)
                              else (detectedTollCharges, detectedTollNames, detectedTollIds, Just Unsure)
            else case (canApplyValidatedPendingToll, validatedPendingToll) of
              (True, Just (pendingCharges, pendingNames, pendingIds)) ->
                (Just pendingCharges, Just pendingNames, Just pendingIds, Just Unsure)
              _ -> (detectedTollCharges, detectedTollNames, detectedTollIds, Nothing)
        else case (detectedTollCharges, canApplyValidatedPendingToll, validatedPendingToll) of
          (Just charges, _, Nothing) ->
            (Just charges, detectedTollNames, detectedTollIds, Just Sure)
          (Just charges, True, Just (pendingCharges, pendingNames, pendingIds)) ->
            -- Some detected + some pending
            let combinedCharges = charges + pendingCharges
                combinedNames = fromMaybe [] detectedTollNames <> pendingNames
                combinedIds = fromMaybe [] detectedTollIds <> pendingIds
             in (Just combinedCharges, Just combinedNames, Just combinedIds, Just Neutral)
          (Nothing, True, Just (pendingCharges, pendingNames, pendingIds)) ->
            (Just pendingCharges, Just pendingNames, Just pendingIds, Just Neutral)
          _ ->
            if maybe False (> 0) estimatedTollCharges
              then (detectedTollCharges, detectedTollNames, detectedTollIds, Just Sure)
              else (detectedTollCharges, detectedTollNames, detectedTollIds, Nothing)
