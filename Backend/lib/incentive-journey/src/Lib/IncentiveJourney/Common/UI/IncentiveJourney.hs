{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Common.UI.IncentiveJourney where

import Data.Aeson (Value)
import qualified Domain.Types.ServiceTierType as ServiceTierType
import qualified Domain.Types.VehicleCategory as VehicleCategory
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Types.TimeBound (TimeBound)
import qualified Lib.IncentiveJourney.Domain.Types.CohortJourneyMapping as DCJM
import qualified Lib.IncentiveJourney.Domain.Types.Common as DIJC
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyMilestone as DIJM
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourneyStats as DIJS
import qualified Lib.IncentiveJourney.Domain.Types.UserCohortMapping as DUCM
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
    -- | Legacy single-number target (RideCompleted / Earnings / …). Ignored for JSONLogic display.
    conditionValue :: Int,
    currentValue :: Int,
    -- | Period progress bag (ride + behaviour + NetOff keys). Compare against evaluation.logic vars.
    currentMetrics :: Maybe Value,
    description :: Maybe Text,
    -- | From milestone template: { "logic": <JsonLogic> }. Null for simple conditionType targets.
    evaluation :: Maybe Value,
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
  { cohortJourneyMappingId :: Id DCJM.CohortJourneyMapping,
    description :: Maybe Text,
    enabled :: Bool,
    endDate :: UTCTime,
    controlFeatures :: Maybe Value,
    periodDays :: Maybe Int,
    journeyId :: Id DIJ.IncentiveJourney,
    journeyType :: DIJ.IncentiveJourneyType,
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
    cohortJourneyMappingId :: Maybe (Id DCJM.CohortJourneyMapping),
    completedAt :: Maybe UTCTime,
    conditionOperator :: DIJM.MilestoneConditionOperator,
    conditionType :: DIJM.MilestoneConditionType,
    conditionValue :: Int,
    currentValue :: Int,
    currentMetrics :: Maybe Value,
    controlFeatures :: Maybe Value,
    -- | Live milestone template evaluation (same as list). Not snapshotted on stats.
    evaluation :: Maybe Value,
    periodDays :: Maybe Int,
    journeyId :: Id DIJ.IncentiveJourney,
    journeyName :: Text,
    journeyType :: DIJ.IncentiveJourneyType,
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

data OptInIncentiveJourneyReq = OptInIncentiveJourneyReq
  { cohortJourneyMappingId :: Id DCJM.CohortJourneyMapping,
    validTill :: UTCTime,
    optInPeriod :: Maybe DUCM.OptInPeriod
  }
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

type IncentiveJourneyOptInAPI =
  ReqBody '[JSON] OptInIncentiveJourneyReq
    :> Post '[JSON] APISuccess.APISuccess
