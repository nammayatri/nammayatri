{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}

module Tools.Rewards.RiderContextReader
  ( readRiderContext,
  )
where

import qualified Data.Aeson as A
import qualified Domain.Types.Person as DP
import Domain.Types.RewardContext (RewardContext (..), rewardContextToLogicInput)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.BehaviourManagement.CustomerCancellationRate as CRWindow
import qualified Storage.Queries.Person as QPerson
import Tools.Error

-- | v1: windowed completed-ride counts plus hasTakenValidRide from Person, and
-- isValidRide for the ride that triggered this evaluation. isValidRide is Nothing
-- (JSON null) when there is no ride context (e.g. dashboard triggerEval) or the BPP
-- did not send the validity tag. Cohort eligibilityJsonLogic can reference
-- ridesLastNd, hasTakenValidRide and isValidRide.
readRiderContext ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) =>
  Id DP.Person ->
  Maybe Bool ->
  m A.Value
readRiderContext personId mbIsValidRide = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  ridesLast1d <- CRWindow.getCompletedCount 1 personId
  ridesLast3d <- CRWindow.getCompletedCount 3 personId
  ridesLast7d <- CRWindow.getCompletedCount 7 personId
  ridesLast30d <- CRWindow.getCompletedCount 30 personId
  ridesLast90d <- CRWindow.getCompletedCount 90 personId
  -- Build the typed context (all fields present) and serialize through the
  -- shared serializer. On all-Just values this reproduces exactly the JSON this
  -- reader has always emitted, so existing cohort logics are unaffected.
  pure $
    rewardContextToLogicInput
      RewardContext
        { ridesLast1d = Just (fromIntegral ridesLast1d),
          ridesLast3d = Just (fromIntegral ridesLast3d),
          ridesLast7d = Just (fromIntegral ridesLast7d),
          ridesLast30d = Just (fromIntegral ridesLast30d),
          ridesLast90d = Just (fromIntegral ridesLast90d),
          hasTakenValidRide = Just person.hasTakenValidRide,
          isValidRide = mbIsValidRide
        }
