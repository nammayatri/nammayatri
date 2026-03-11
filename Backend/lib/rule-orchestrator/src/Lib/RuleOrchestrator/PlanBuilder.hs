{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.RuleOrchestrator.PlanBuilder
  ( buildPlan,
    defaultPlan,
  )
where

import Lib.BehaviorTracker.Types (BehaviorSnapshot)
import Lib.RuleOrchestrator.Types
import qualified Lib.Yudhishthira.Types as LYT

-- | Build the rule execution plan for a given snapshot
-- Currently returns the default 3-step plan for all action types.
-- Can be extended to return action-specific plans in the future.
buildPlan :: BehaviorSnapshot -> RuleExecutionPlan
buildPlan _snapshot = defaultPlan

-- | Default 3-step plan: threshold → consequence → communication
--
-- Step 1: BEHAVIOR_THRESHOLD_CHECK — "Should we act?"
--   Input: snapshot counters + entity state
--   Output: { breached: true/false, severity: "HIGH"/"MEDIUM"/"LOW" }
--
-- Step 2: BEHAVIOR_CONSEQUENCE_CALC — "What consequence?"
--   Input: snapshot + step 1 output
--   Output: { consequences: [{ consequenceType: "...", params: {...}, requiresResolution: true/false }] }
--
-- Step 3: BEHAVIOR_COMMUNICATION — "How to communicate?"
--   Input: snapshot + step 2 output
--   Output: { communications: [{ channel: "...", templateKey: "...", params: {...}, delaySeconds: 0 }] }
defaultPlan :: RuleExecutionPlan
defaultPlan =
  RuleExecutionPlan
    { steps =
        [ RuleStep
            { stepName = "threshold_check",
              domain = LYT.BEHAVIOR_THRESHOLD_CHECK,
              priority = 1,
              dependsOn = []
            },
          RuleStep
            { stepName = "consequence_calc",
              domain = LYT.BEHAVIOR_CONSEQUENCE_CALC,
              priority = 2,
              dependsOn = ["threshold_check"]
            },
          RuleStep
            { stepName = "communication",
              domain = LYT.BEHAVIOR_COMMUNICATION,
              priority = 3,
              dependsOn = ["consequence_calc"]
            }
        ]
    }
