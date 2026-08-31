{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Common.UI.IncentiveJourney where

import qualified Domain.Types.ServiceTierType as ServiceTierType
import qualified Domain.Types.VehicleCategory as VehicleCategory
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.TimeBound (TimeBound)
import qualified Lib.IncentiveJourney.Domain.Types.Common as DIJC
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import Servant

data SubscriptionWaiveOffConfig = SubscriptionWaiveOffConfig
  { daysValidFor :: Int,
    percentage :: Int,
    serviceName :: Text,
    waiveOffMode :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyMilestoneItem = IncentiveJourneyMilestoneItem
  { areaType :: Maybe DIJM.MilestoneAreaType,
    conditionOperator :: DIJM.MilestoneConditionOperator,
    conditionType :: DIJM.MilestoneConditionType,
    conditionValue :: Int,
    currentValue :: Int,
    description :: Maybe Text,
    milestoneId :: Id DIJM.IncentiveJourneyMilestone,
    name :: Maybe Text,
    order :: Int,
    rewardType :: DIJC.MilestoneRewardType,
    rewardValue :: Maybe Int,
    serviceTierType :: Maybe ServiceTierType.ServiceTierType,
    specialLocationNames :: Maybe [Text],
    status :: DIJS.JourneyMilestoneStatus,
    subscriptionWaiveOff :: Maybe SubscriptionWaiveOffConfig,
    timeBounds :: Maybe TimeBound,
    vehicleCategory :: Maybe VehicleCategory.VehicleCategory
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyListItem = IncentiveJourneyListItem
  { description :: Maybe Text,
    enabled :: Bool,
    endDate :: UTCTime,
    journeyId :: Id DIJ.IncentiveJourney,
    journeyType :: Maybe DIJ.IncentiveJourneyType,
    milestones :: [IncentiveJourneyMilestoneItem],
    name :: Text,
    startDate :: UTCTime,
    streakRange :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyListRes = IncentiveJourneyListRes {journeys :: [IncentiveJourneyListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyHistoryItem = IncentiveJourneyHistoryItem
  { areaType :: Maybe DIJM.MilestoneAreaType,
    completedAt :: Maybe UTCTime,
    conditionOperator :: DIJM.MilestoneConditionOperator,
    conditionType :: DIJM.MilestoneConditionType,
    conditionValue :: Int,
    currentValue :: Int,
    journeyId :: Id DIJ.IncentiveJourney,
    journeyName :: Text,
    journeyType :: Maybe DIJ.IncentiveJourneyType,
    milestoneDescription :: Maybe Text,
    milestoneId :: Id DIJM.IncentiveJourneyMilestone,
    milestoneName :: Maybe Text,
    milestoneOrder :: Int,
    periodKey :: Text,
    rewardType :: DIJC.MilestoneRewardType,
    rewardValue :: Maybe Int,
    specialLocationNames :: Maybe [Text],
    status :: DIJS.JourneyMilestoneStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IncentiveJourneyHistoryRes = IncentiveJourneyHistoryRes {history :: [IncentiveJourneyHistoryItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type IncentiveJourneyListAPI =
  QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] IncentiveJourneyListRes

type IncentiveJourneyHistoryAPI =
  QueryParam "date" Text
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] IncentiveJourneyHistoryRes
