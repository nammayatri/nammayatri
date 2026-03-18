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
-- Looks for "consequences" key in step results -> parses into [ConsequenceDirective]
-- Looks for "communications" key in step results -> parses into [CommunicationDirective]
-- Falls back to empty lists if keys not found (graceful degradation)
-- Parse errors are recorded in the corresponding StepResult.errors for observability.
mergeResults :: [StepResult] -> OrchestratedOutput
mergeResults results =
  let annotated = map annotateParseErrors results
   in OrchestratedOutput
        { consequences = concatMap (fst . extractConsequences) results,
          communications = concatMap (fst . extractCommunications) results,
          stepResults = annotated
        }

-- | Annotate a StepResult with any parse errors encountered during extraction
annotateParseErrors :: StepResult -> StepResult
annotateParseErrors result =
  let (_, cErrs) = extractConsequences result
      (_, mErrs) = extractCommunications result
      newErrors = result.errors <> cErrs <> mErrs
   in result {errors = newErrors}

-- | Extract consequence directives from a step's output, returning parse errors
extractConsequences :: StepResult -> ([ConsequenceDirective], [String])
extractConsequences result =
  case result.output of
    A.Object obj ->
      case KM.lookup (AK.fromText "consequences") obj of
        Just val -> parseOrReport "consequences" result.stepName val
        Nothing -> ([], [])
    _ -> ([], [])

-- | Extract communication directives from a step's output, returning parse errors
extractCommunications :: StepResult -> ([CommunicationDirective], [String])
extractCommunications result =
  case result.output of
    A.Object obj ->
      case KM.lookup (AK.fromText "communications") obj of
        Just val -> parseOrReport "communications" result.stepName val
        Nothing -> ([], [])
    _ -> ([], [])

-- | Try to parse a JSON value; on failure return empty list and an error message
parseOrReport :: (A.FromJSON a) => String -> Text -> A.Value -> ([a], [String])
parseOrReport fieldName stepName val =
  case A.fromJSON val of
    A.Success directives -> (directives, [])
    A.Error err -> ([], ["Failed to parse " <> fieldName <> " in step " <> show stepName <> ": " <> err])
