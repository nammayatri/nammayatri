{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.RuleOrchestrator.Types
  ( RuleExecutionPlan (..),
    RuleStep (..),
    StepResult (..),
    OrchestratedOutput (..),
    ConsequenceDirective (..),
    CommunicationDirective (..),
    defaultOrchestratedOutput,
    defaultStepResult,
  )
where

import Data.Aeson (Value (..))
import Kernel.Prelude
import qualified Lib.Yudhishthira.Types as LYT

-- | A plan of which rule domains to invoke and in what order
data RuleExecutionPlan = RuleExecutionPlan
  { steps :: [RuleStep]
  }
  deriving (Show, Generic)

-- | A single step in the rule execution plan
data RuleStep = RuleStep
  { stepName :: Text, -- human-readable: "threshold_check", "consequence_calc", "communication"
    domain :: LYT.LogicDomain, -- which Yudhishthira domain to query
    priority :: Int, -- execution order (lower = first)
    dependsOn :: [Text] -- stepNames that must complete before this step runs
  }
  deriving (Show, Generic)

-- | Output from a single rule step execution
data StepResult = StepResult
  { stepName :: Text,
    domain :: LYT.LogicDomain,
    output :: Value, -- raw JSON output from rule engine
    errors :: [String],
    version :: Maybe Int -- rule version used
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Default empty step result
defaultStepResult :: Text -> LYT.LogicDomain -> StepResult
defaultStepResult name domain =
  StepResult
    { stepName = name,
      domain = domain,
      output = Null,
      errors = [],
      version = Nothing
    }

-- | Final merged output from all rule steps — consumed by consequence + communication engines
data OrchestratedOutput = OrchestratedOutput
  { consequences :: [ConsequenceDirective],
    communications :: [CommunicationDirective],
    stepResults :: [StepResult] -- full audit trace of all steps
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Default empty output
defaultOrchestratedOutput :: OrchestratedOutput
defaultOrchestratedOutput =
  OrchestratedOutput
    { consequences = [],
      communications = [],
      stepResults = []
    }

-- | Directive for the consequence-engine
-- Uses Text for consequenceType so rule engine defines the values via JSON Logic,
-- keeping rule output decoupled from code-level enums.
data ConsequenceDirective = ConsequenceDirective
  { consequenceType :: Text, -- "NUDGE", "SOFT_BLOCK", "CHARGE_FEE", "HARD_BLOCK", etc.
    params :: Value, -- type-specific params (blockDuration, chargeAmount, etc.)
    requiresResolution :: Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Directive for the communication-engine
data CommunicationDirective = CommunicationDirective
  { channel :: Text, -- "FCM_NOTIFICATION", "IN_APP_OVERLAY", "SMS", etc.
    templateKey :: Text, -- template identifier for the message
    params :: Value, -- template variables
    delaySeconds :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)
