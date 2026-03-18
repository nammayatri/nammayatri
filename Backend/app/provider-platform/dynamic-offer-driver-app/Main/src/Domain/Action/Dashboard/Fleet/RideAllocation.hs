{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Fleet.RideAllocation
  ( listAllocationPolicies,
    getAllocationPolicy,
    createAllocationPolicy,
    updateAllocationPolicy,
    activateAllocationPolicy,
    deactivateAllocationPolicy,
    simulateAllocationPolicy,
    getAllocationPolicyHistory,
    getAllocationMonitoring,
  )
where

import qualified Domain.Types.Dashboard.RideAllocation as DRA
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common

-- | List allocation policies with filters for a given merchant and city
listAllocationPolicies ::
  Text ->
  Text ->
  Maybe DRA.AllocationPolicyStatus ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow DRA.AllocationPoliciesResponse
listAllocationPolicies merchantId cityId _mbStatus _mbSearch _mbLimit _mbOffset = do
  logInfo $ "Listing allocation policies for merchant: " <> merchantId <> " city: " <> cityId
  -- TODO: Implement DB query via Storage.Queries.RideAllocationPolicy
  pure $
    DRA.AllocationPoliciesResponse
      { policies = [],
        totalCount = 0
      }

-- | Get a single allocation policy by ID with full rule and weight configuration
getAllocationPolicy ::
  Text ->
  Text ->
  Text ->
  Flow DRA.AllocationPolicy
getAllocationPolicy merchantId _cityId policyId = do
  logInfo $ "Getting allocation policy: " <> policyId <> " for merchant: " <> merchantId
  -- TODO: Implement DB query via Storage.Queries.RideAllocationPolicy
  throwError $ InvalidRequest "Policy not found"

-- | Create a new allocation policy with rules and weights
createAllocationPolicy ::
  Text ->
  Text ->
  DRA.CreateAllocationPolicyRequest ->
  Flow DRA.CreateAllocationPolicyResponse
createAllocationPolicy merchantId cityId req = do
  logInfo $ "Creating allocation policy: " <> req.name <> " for merchant: " <> merchantId <> " city: " <> cityId
  now <- getCurrentTime
  policyId <- generateGUID
  -- TODO: Validate weights sum to 100
  -- TODO: Insert policy, rules, weights into DB
  -- TODO: Record history entry (CREATED)
  pure $
    DRA.CreateAllocationPolicyResponse
      { policyId = policyId,
        status = req.status,
        createdAt = now
      }

-- | Update an existing allocation policy (rules, weights, metadata)
updateAllocationPolicy ::
  Text ->
  Text ->
  Text ->
  DRA.UpdateAllocationPolicyRequest ->
  Flow ()
updateAllocationPolicy merchantId _cityId policyId _req = do
  logInfo $ "Updating allocation policy: " <> policyId <> " for merchant: " <> merchantId
  -- TODO: Fetch existing policy, validate version for optimistic locking
  -- TODO: Update policy, rules, weights in DB
  -- TODO: Record history entry (UPDATED) with previous and new state snapshots
  pure ()

-- | Activate an allocation policy after conflict validation
activateAllocationPolicy ::
  Text ->
  Text ->
  Text ->
  Flow ()
activateAllocationPolicy merchantId cityId policyId = do
  logInfo $ "Activating allocation policy: " <> policyId <> " for merchant: " <> merchantId <> " city: " <> cityId
  -- TODO: Fetch policy, validate it exists and is not already ACTIVE
  -- TODO: Check for conflicting active policies in the same city/zone
  -- TODO: Update policy status to ACTIVE
  -- TODO: Record history entry (ACTIVATED)
  pure ()

-- | Deactivate an active allocation policy
deactivateAllocationPolicy ::
  Text ->
  Text ->
  Text ->
  Flow ()
deactivateAllocationPolicy merchantId _cityId policyId = do
  logInfo $ "Deactivating allocation policy: " <> policyId <> " for merchant: " <> merchantId
  -- TODO: Fetch policy, validate it exists and is ACTIVE
  -- TODO: Update policy status to INACTIVE
  -- TODO: Record history entry (DEACTIVATED)
  pure ()

-- | Run a dry-run simulation of a policy against historical ride data
simulateAllocationPolicy ::
  Text ->
  Text ->
  Text ->
  DRA.SimulateAllocationPolicyRequest ->
  Flow DRA.AllocationSimulationResult
simulateAllocationPolicy merchantId _cityId policyId req = do
  logInfo $ "Simulating allocation policy: " <> policyId <> " for merchant: " <> merchantId <> " sample: " <> show req.sampleSize
  -- TODO: Fetch policy with rules and weights
  -- TODO: Query historical ride data for the simulation period
  -- TODO: Apply policy rules to each ride, compute allocation distribution
  pure $
    DRA.AllocationSimulationResult
      { totalRidesSimulated = 0,
        simulationPeriod = req.simulationPeriod,
        distribution = [],
        rulesMatchRate = 0.0,
        unallocatedRides = 0
      }

-- | Get the audit trail/history of changes for a policy
getAllocationPolicyHistory ::
  Text ->
  Text ->
  Text ->
  Flow DRA.AllocationPolicyHistoryResponse
getAllocationPolicyHistory merchantId _cityId policyId = do
  logInfo $ "Getting allocation policy history: " <> policyId <> " for merchant: " <> merchantId
  -- TODO: Query ride_allocation_policy_history table
  pure $
    DRA.AllocationPolicyHistoryResponse
      { history = [],
        totalCount = 0
      }

-- | Get allocation monitoring data from ClickHouse for a city and date range
getAllocationMonitoring ::
  Text ->
  Text ->
  Text ->
  Text ->
  Flow DRA.AllocationMonitoringResponse
getAllocationMonitoring merchantId cityId fromDate toDate = do
  logInfo $ "Getting allocation monitoring for merchant: " <> merchantId <> " city: " <> cityId <> " from: " <> fromDate <> " to: " <> toDate
  -- TODO: Query ClickHouse ride_allocation_outcomes table
  -- TODO: Aggregate by policy, fleet owner, time bucket
  pure $
    DRA.AllocationMonitoringResponse
      { summary =
          DRA.AllocationMonitoringSummary
            { totalRidesAllocated = 0,
              avgAllocationTimeMs = 0,
              policyHitRate = 0.0,
              fleetUtilizationBalance = 0.0
            },
        policyEffectiveness = [],
        timeSeries = []
      }
