{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Eval
  ( MilestoneStore (..),
    evaluateMilestonesInOrder,
    evaluateMilestonesInOrderWith,
  )
where

import Kernel.Prelude
import Lib.IncentiveJourney.Condition
import Lib.IncentiveJourney.Reward
import Lib.IncentiveJourney.Types
import qualified Lib.IncentiveJourney.Window as Window

data MilestoneStore m = MilestoneStore
  { loadStats :: EvalMilestone -> m (Maybe EvalStats),
    persistProgress :: EvalMilestone -> Int -> m (),
    persistCompleted :: EvalMilestone -> Int -> Int -> m (),
    onCompleted :: EvalMilestone -> Int -> m ()
  }

evaluateMilestonesInOrder ::
  (Monad m, RewardDispatcher m) =>
  UTCTime ->
  RewardDispatchCtx ->
  RideDeltas ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  MilestoneStore m ->
  [EvalMilestone] ->
  m ()
evaluateMilestonesInOrder localTime =
  evaluateMilestonesInOrderWith localTime dispatchReward

evaluateMilestonesInOrderWith ::
  Monad m =>
  UTCTime ->
  (RewardDispatchCtx -> RewardSpec -> m AwardResult) ->
  RewardDispatchCtx ->
  RideDeltas ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  MilestoneStore m ->
  [EvalMilestone] ->
  m ()
evaluateMilestonesInOrderWith _ _ _ _ _ _ _ _ _ [] = pure ()
evaluateMilestonesInOrderWith localTime dispatch ctx deltas mbPickup mbDrop mbVehCat mbServiceTier store (milestone : rest) = do
  mbExisting <- store.loadStats milestone
  if maybe False (isTerminalStatus . (.status)) mbExisting
    then continueRest
    else -- Outside peak / vehicle / location: no delta and do not advance sequential chain.

      if not (Window.isWithinTimeBound localTime milestone.timeBounds)
        || not (rideMatchesVehicleFilter milestone mbVehCat mbServiceTier)
        || not (rideMatchesLocationFilter milestone mbPickup mbDrop)
        then pure ()
        else evaluateMatchingMilestone mbExisting
  where
    continueRest =
      evaluateMilestonesInOrderWith localTime dispatch ctx deltas mbPickup mbDrop mbVehCat mbServiceTier store rest

    evaluateMatchingMilestone mbExisting = do
      let delta = deltaForCondition milestone.conditionType deltas
          prevValue = maybe 0 (.currentValue) mbExisting
          currentValue = prevValue + delta
          conditionMet =
            evaluateCondition
              (conditionOperatorOrDefault milestone.conditionOperator)
              currentValue
              milestone.conditionValue
      if not conditionMet
        then store.persistProgress milestone currentValue
        else do
          let ctx' :: RewardDispatchCtx
              ctx' = ctx {milestoneId = milestone.milestoneId}
          awardedResult <- dispatch ctx' milestone.rewardSpec
          -- AwardSkipped keeps InProgress (retry). Awarded n (incl. 0 for NoReward) completes.
          case awardedResult of
            AwardSkipped -> store.persistProgress milestone currentValue
            Awarded awarded -> do
              store.persistCompleted milestone currentValue awarded
              store.onCompleted milestone awarded
