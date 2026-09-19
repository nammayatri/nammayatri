{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Types
  ( JourneyPeriodType (..),
    ConditionType (..),
    ConditionOperator (..),
    AreaType (..),
    RewardKind (..),
    MilestoneStatus (..),
    RideDeltas (..),
    RewardSpec (..),
    SubscriptionWaiveOffSpec (..),
    SubscriptionWaiveOffMetadata (..),
    encodeSubscriptionWaiveOffMetadata,
    mkSubscriptionWaiveOffSpec,
    RewardDispatchCtx (..),
    AwardResult (..),
    EvalMilestone (..),
    EvalStats (..),
    awardAmount,
    conditionOperatorOrDefault,
    isTerminalStatus,
    isStreakEndSatisfiedStatus,
  )
where

import Data.Aeson (Value, object, withObject, (.:), (.=))
import qualified Data.Aeson as A
import Kernel.Prelude
import qualified Kernel.Types.TimeBound as TB

data JourneyPeriodType
  = Daily
  | Weekly
  | Monthly
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ConditionType
  = RideCompleted
  | Earnings
  | Distance
  | RideDuration
  | BookingTicket
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ConditionOperator
  = GTE
  | GT
  | EQ
  | LTE
  | LT
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Special-location area filter (independent of condition type).
data AreaType
  = Default
  | Pickup
  | Drop
  | PickupDrop
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data RewardKind
  = Coins
  | Cash
  | Coupons
  | WalletMoney
  | SubscriptionWaiveOff
  | PoolingPriority
  | NoReward
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data MilestoneStatus
  = NotStarted
  | InProgress
  | Completed
  | Rewarded
  | WaivedOff
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data RideDeltas = RideDeltas
  { ridesDelta :: Int,
    earningsDelta :: Int,
    distanceMetersDelta :: Int,
    rideTimeSecondsDelta :: Int
  }
  deriving (Eq, Show, Generic)

-- | Runtime SWO payload. Persisted as:
--   rewardValue / streakEndRewardValue           = percentage
--   rewardExpirationAt / streakEndRewardExpirationAt = daysValidFor
--   rewardMetadata / streakEndRewardMetadata     = {serviceName, waiveOffMode}
data SubscriptionWaiveOffSpec = SubscriptionWaiveOffSpec
  { percentage :: Int,
    daysValidFor :: Int,
    serviceName :: Text,
    waiveOffMode :: Text
  }
  deriving (Eq, Show, Generic)

-- | JSON body stored in reward_metadata / streak_end_reward_metadata for SubscriptionWaiveOff.
data SubscriptionWaiveOffMetadata = SubscriptionWaiveOffMetadata
  { serviceName :: Text,
    waiveOffMode :: Text
  }
  deriving (Eq, Show, Generic)

instance A.FromJSON SubscriptionWaiveOffMetadata where
  parseJSON = withObject "SubscriptionWaiveOffMetadata" $ \o ->
    SubscriptionWaiveOffMetadata
      <$> o .: "serviceName"
      <*> o .: "waiveOffMode"

instance A.ToJSON SubscriptionWaiveOffMetadata where
  toJSON SubscriptionWaiveOffMetadata {..} =
    object ["serviceName" .= serviceName, "waiveOffMode" .= waiveOffMode]

encodeSubscriptionWaiveOffMetadata :: Text -> Text -> Value
encodeSubscriptionWaiveOffMetadata serviceName waiveOffMode =
  A.toJSON SubscriptionWaiveOffMetadata {serviceName = serviceName, waiveOffMode = waiveOffMode}

-- | Build SWO from shared value/expiration/metadata columns. All three must be present and valid.
mkSubscriptionWaiveOffSpec :: Maybe Int -> Maybe Int -> Maybe Value -> Maybe SubscriptionWaiveOffSpec
mkSubscriptionWaiveOffSpec mbPercentage mbDaysValidFor mbMetadata = do
  percentage <- mbPercentage
  daysValidFor <- mbDaysValidFor
  metaVal <- mbMetadata
  SubscriptionWaiveOffMetadata {serviceName, waiveOffMode} <- case A.fromJSON metaVal of
    A.Success meta -> Just meta
    A.Error _ -> Nothing
  pure
    SubscriptionWaiveOffSpec
      { percentage = percentage,
        daysValidFor = daysValidFor,
        serviceName = serviceName,
        waiveOffMode = waiveOffMode
      }

data RewardSpec = RewardSpec
  { rewardKind :: RewardKind,
    rewardValue :: Maybe Int,
    rewardExpirationAt :: Maybe Int,
    subscriptionWaiveOff :: Maybe SubscriptionWaiveOffSpec
  }
  deriving (Eq, Show, Generic)

data RewardDispatchCtx = RewardDispatchCtx
  { personId :: Text,
    journeyName :: Text,
    milestoneId :: Text
  }
  deriving (Eq, Show, Generic)

data AwardResult
  = AwardSkipped
  | Awarded Int
  deriving (Eq, Show, Generic)

data EvalMilestone = EvalMilestone
  { milestoneId :: Text,
    conditionType :: ConditionType,
    conditionOperator :: Maybe ConditionOperator,
    conditionValue :: Int,
    -- | Nothing / Default = no location filter. Pickup/Drop/PickupDrop use specialLocationIds.
    areaType :: Maybe AreaType,
    specialLocationIds :: Maybe [Text],
    -- | Shown VehicleCategory / ServiceTierType; Nothing = any.
    vehicleCategory :: Maybe Text,
    serviceTierType :: Maybe Text,
    timeBounds :: Maybe TB.TimeBound,
    rewardSpec :: RewardSpec
  }
  deriving (Eq, Show, Generic)

data EvalStats = EvalStats
  { currentValue :: Int,
    status :: MilestoneStatus
  }
  deriving (Eq, Show, Generic)

awardAmount :: AwardResult -> Int
awardAmount AwardSkipped = 0
awardAmount (Awarded n) = n

conditionOperatorOrDefault :: Maybe ConditionOperator -> ConditionOperator
conditionOperatorOrDefault = fromMaybe GTE

-- | Milestone is done for sequencing (no further progress).
isTerminalStatus :: MilestoneStatus -> Bool
isTerminalStatus status = status `elem` [Completed, Rewarded, WaivedOff]

-- | Counts toward streak-end payout eligibility.
isStreakEndSatisfiedStatus :: MilestoneStatus -> Bool
isStreakEndSatisfiedStatus status = status `elem` [Completed, Rewarded, WaivedOff]
