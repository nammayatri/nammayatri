{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.RuleOrchestrator.OutputMerger
  ( mergeResults,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Kernel.Prelude
import Lib.RuleOrchestrator.Types

-- | Merge step results into a single OrchestratedOutput
--
-- Looks for "consequences" key in step results → parses into [ConsequenceDirective]
-- Looks for "communications" key in step results → parses into [CommunicationDirective]
-- Falls back to empty lists if keys not found (graceful degradation)
mergeResults :: [StepResult] -> OrchestratedOutput
mergeResults results =
  OrchestratedOutput
    { consequences = concatMap extractConsequences results,
      communications = concatMap extractCommunications results,
      stepResults = results
    }

-- | Extract consequence directives from a step's output
extractConsequences :: StepResult -> [ConsequenceDirective]
extractConsequences result =
  case result.output of
    A.Object obj ->
      case KM.lookup (AK.fromText "consequences") obj of
        Just val -> parseOrEmpty val
        Nothing -> []
    _ -> []
  where
    parseOrEmpty val =
      case A.fromJSON val of
        A.Success directives -> directives
        A.Error _ -> []

-- | Extract communication directives from a step's output
extractCommunications :: StepResult -> [CommunicationDirective]
extractCommunications result =
  case result.output of
    A.Object obj ->
      case KM.lookup (AK.fromText "communications") obj of
        Just val -> parseOrEmpty val
        Nothing -> []
    _ -> []
  where
    parseOrEmpty val =
      case A.fromJSON val of
        A.Success directives -> directives
        A.Error _ -> []
