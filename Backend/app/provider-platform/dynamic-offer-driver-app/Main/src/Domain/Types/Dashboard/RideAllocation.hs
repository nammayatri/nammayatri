{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Dashboard.RideAllocation
  ( AllocationPolicy (..),
    AllocationPolicyStatus (..),
    AllocationRule (..),
    ConditionType (..),
    RuleOperator (..),
    AllocationWeight (..),
    AllocationPolicyHistory (..),
    PolicyAction (..),
    AllocationSimulationResult (..),
    SimulationDistribution (..),
    AllocationMonitoringSummary (..),
    PolicyEffectiveness (..),
    MonitoringTimeSeries (..),
    AllocationMonitoringResponse (..),
    AllocationPoliciesResponse (..),
    AllocationPolicySummary (..),
    AllocationPolicyHistoryResponse (..),
    HistoryEntry (..),
    CreateAllocationPolicyRequest (..),
    UpdateAllocationPolicyRequest (..),
    SimulateAllocationPolicyRequest (..),
    CreateAllocationPolicyResponse (..),
    CreateAllocationRuleRequest (..),
    CreateAllocationWeightRequest (..),
  )
where

import Data.Aeson (Value)
import Data.Time (UTCTime)
import Kernel.Prelude
import Kernel.Types.Id

-- ============================================
-- Enums
-- ============================================

data AllocationPolicyStatus = DRAFT | ACTIVE | INACTIVE | ARCHIVED
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data ConditionType
  = ZONE
  | VEHICLE_VARIANT
  | TIME_WINDOW
  | FLEET_PERFORMANCE_TIER
  | CAPACITY_UTILIZATION
  | DRIVER_RATING_MIN
  | FLEET_SIZE_MIN
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data RuleOperator = EQUALS | NOT_EQUALS | IN | NOT_IN | GREATER_THAN | LESS_THAN | BETWEEN
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data PolicyAction = CREATED | UPDATED | ACTIVATED | DEACTIVATED
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

-- ============================================
-- Domain Entities
-- ============================================

data AllocationPolicy = AllocationPolicy
  { id :: Id AllocationPolicy,
    name :: Text,
    description :: Maybe Text,
    merchantId :: Text,
    cityId :: Text,
    status :: AllocationPolicyStatus,
    priority :: Int,
    effectiveFrom :: Maybe UTCTime,
    effectiveTo :: Maybe UTCTime,
    createdBy :: Text,
    updatedBy :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    version :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AllocationRule = AllocationRule
  { id :: Id AllocationRule,
    policyId :: Id AllocationPolicy,
    ruleOrder :: Int,
    conditionType :: ConditionType,
    operator :: RuleOperator,
    value :: Value,
    createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AllocationWeight = AllocationWeight
  { id :: Id AllocationWeight,
    policyId :: Id AllocationPolicy,
    fleetOwnerId :: Maybe Text,
    fleetTier :: Maybe Text,
    weight :: Double,
    maxRidesPerHour :: Maybe Int,
    createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AllocationPolicyHistory = AllocationPolicyHistory
  { id :: Id AllocationPolicyHistory,
    policyId :: Id AllocationPolicy,
    action :: PolicyAction,
    changedBy :: Text,
    previousState :: Maybe Value,
    newState :: Maybe Value,
    changeSummary :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- ============================================
-- API Request/Response Types
-- ============================================

data AllocationPolicySummary = AllocationPolicySummary
  { policyId :: Text,
    name :: Text,
    description :: Maybe Text,
    status :: AllocationPolicyStatus,
    priority :: Int,
    cityName :: Text,
    rulesCount :: Int,
    fleetOwnersAffected :: Int,
    effectiveFrom :: Maybe UTCTime,
    effectiveTo :: Maybe UTCTime,
    updatedAt :: UTCTime,
    updatedBy :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AllocationPoliciesResponse = AllocationPoliciesResponse
  { policies :: [AllocationPolicySummary],
    totalCount :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data CreateAllocationRuleRequest = CreateAllocationRuleRequest
  { ruleOrder :: Int,
    conditionType :: ConditionType,
    operator :: RuleOperator,
    value :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data CreateAllocationWeightRequest = CreateAllocationWeightRequest
  { fleetOwnerId :: Maybe Text,
    fleetTier :: Maybe Text,
    weight :: Double,
    maxRidesPerHour :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data CreateAllocationPolicyRequest = CreateAllocationPolicyRequest
  { name :: Text,
    description :: Maybe Text,
    cityId :: Text,
    priority :: Int,
    effectiveFrom :: Maybe UTCTime,
    effectiveTo :: Maybe UTCTime,
    rules :: [CreateAllocationRuleRequest],
    weights :: [CreateAllocationWeightRequest],
    status :: AllocationPolicyStatus
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data UpdateAllocationPolicyRequest = UpdateAllocationPolicyRequest
  { name :: Maybe Text,
    description :: Maybe Text,
    priority :: Maybe Int,
    effectiveFrom :: Maybe UTCTime,
    effectiveTo :: Maybe UTCTime,
    rules :: Maybe [CreateAllocationRuleRequest],
    weights :: Maybe [CreateAllocationWeightRequest]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data CreateAllocationPolicyResponse = CreateAllocationPolicyResponse
  { policyId :: Text,
    status :: AllocationPolicyStatus,
    createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data SimulateAllocationPolicyRequest = SimulateAllocationPolicyRequest
  { simulationPeriod :: Text,
    sampleSize :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data SimulationDistribution = SimulationDistribution
  { fleetOwnerId :: Text,
    fleetOwnerName :: Text,
    configuredWeight :: Double,
    simulatedShare :: Double,
    simulatedRides :: Int,
    deviation :: Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AllocationSimulationResult = AllocationSimulationResult
  { totalRidesSimulated :: Int,
    simulationPeriod :: Text,
    distribution :: [SimulationDistribution],
    rulesMatchRate :: Double,
    unallocatedRides :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data HistoryEntry = HistoryEntry
  { historyId :: Text,
    action :: PolicyAction,
    changedBy :: Text,
    changeSummary :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AllocationPolicyHistoryResponse = AllocationPolicyHistoryResponse
  { history :: [HistoryEntry],
    totalCount :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AllocationMonitoringSummary = AllocationMonitoringSummary
  { totalRidesAllocated :: Int,
    avgAllocationTimeMs :: Int,
    policyHitRate :: Double,
    fleetUtilizationBalance :: Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data PolicyEffectiveness = PolicyEffectiveness
  { policyId :: Text,
    policyName :: Text,
    totalRidesMatched :: Int,
    expectedDistribution :: Value,
    actualDistribution :: Value,
    maxDeviation :: Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data MonitoringTimeSeries = MonitoringTimeSeries
  { timestamp :: UTCTime,
    ridesAllocated :: Int,
    avgAllocationMs :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data AllocationMonitoringResponse = AllocationMonitoringResponse
  { summary :: AllocationMonitoringSummary,
    policyEffectiveness :: [PolicyEffectiveness],
    timeSeries :: [MonitoringTimeSeries]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
