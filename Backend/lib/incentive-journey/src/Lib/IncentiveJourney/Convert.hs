{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Convert
  ( journeyTypeOrDefault,
    toLibPeriodType,
    toEvalMilestone,
    toEvalStats,
    toLibConditionType,
    toLibConditionOperator,
    toLibAreaType,
    toLibRewardKind,
    toLibStatus,
  )
where

import Kernel.Prelude
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import qualified Lib.IncentiveJourney.Types as IJ

journeyTypeOrDefault :: Maybe DIJ.IncentiveJourneyType -> DIJ.IncentiveJourneyType
journeyTypeOrDefault = fromMaybe DIJ.Daily

toLibPeriodType :: DIJ.IncentiveJourneyType -> IJ.JourneyPeriodType
toLibPeriodType = \case
  DIJ.Daily -> IJ.Daily
  DIJ.Weekly -> IJ.Weekly
  DIJ.Monthly -> IJ.Monthly

toEvalMilestone :: DIJM.IncentiveJourneyMilestone -> IJ.EvalMilestone
toEvalMilestone milestone =
  IJ.EvalMilestone
    { milestoneId = milestone.id.getId,
      conditionType = toLibConditionType milestone.conditionType,
      conditionOperator = toLibConditionOperator <$> milestone.conditionOperator,
      conditionValue = milestone.conditionValue,
      areaType = toLibAreaType <$> milestone.areaType,
      specialLocationIds = milestone.specialLocationIds,
      vehicleCategory = show <$> milestone.vehicleCategory,
      serviceTierType = show <$> milestone.serviceTierType,
      timeBounds = milestone.timeBounds,
      rewardSpec =
        IJ.RewardSpec
          { rewardKind = toLibRewardKind milestone.rewardType,
            rewardValue = milestone.rewardValue,
            rewardExpirationAt = milestone.rewardExpirationAt
          }
    }

toEvalStats :: DIJS.IncentiveJourneyStats -> IJ.EvalStats
toEvalStats stats =
  IJ.EvalStats
    { currentValue = stats.currentValue,
      status = toLibStatus stats.status
    }

toLibConditionType :: DIJM.MilestoneConditionType -> IJ.ConditionType
toLibConditionType = \case
  DIJM.RideCompleted -> IJ.RideCompleted
  DIJM.Earnings -> IJ.Earnings
  DIJM.Distance -> IJ.Distance
  DIJM.RideDuration -> IJ.RideDuration
  DIJM.BookingTicket -> IJ.BookingTicket

toLibConditionOperator :: DIJM.MilestoneConditionOperator -> IJ.ConditionOperator
toLibConditionOperator = \case
  DIJM.GTE -> IJ.GTE
  DIJM.GT -> IJ.GT
  DIJM.EQ -> IJ.EQ
  DIJM.LTE -> IJ.LTE
  DIJM.LT -> IJ.LT

toLibAreaType :: DIJM.MilestoneAreaType -> IJ.AreaType
toLibAreaType = \case
  DIJM.Default -> IJ.Default
  DIJM.Pickup -> IJ.Pickup
  DIJM.Drop -> IJ.Drop
  DIJM.PickupDrop -> IJ.PickupDrop

toLibRewardKind :: Common.MilestoneRewardType -> IJ.RewardKind
toLibRewardKind = \case
  Common.Coins -> IJ.Coins
  Common.Cash -> IJ.Cash
  Common.Coupons -> IJ.Coupons
  Common.WalletMoney -> IJ.WalletMoney
  Common.SubscriptionWaiveOff -> IJ.SubscriptionWaiveOff
  Common.PoolingPriority -> IJ.PoolingPriority
  Common.NoReward -> IJ.NoReward

toLibStatus :: DIJS.JourneyMilestoneStatus -> IJ.MilestoneStatus
toLibStatus = \case
  DIJS.NotStarted -> IJ.NotStarted
  DIJS.InProgress -> IJ.InProgress
  DIJS.Completed -> IJ.Completed
  DIJS.Rewarded -> IJ.Rewarded
  DIJS.WaivedOff -> IJ.WaivedOff
