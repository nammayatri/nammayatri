{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.BehaviorEngine.RuleEvaluator
  ( evaluateRules,
    RuleFetcher,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config (ClickhouseFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.BehaviorEngine.Types
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow, HasYudhishthiraTablesSchema)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT

-- | Function type for fetching rules — provided by the app
-- Apps implement this using Tools.DynamicLogic.getAppDynamicLogic
type RuleFetcher m = LYT.LogicDomain -> m ([A.Value], Maybe Int)

-- | Evaluate rules for a given snapshot
--
-- Uses a single LogicDomain that returns both consequences and communications
-- in one evaluation. The RuleFetcher is provided by the app (wrapping
-- Tools.DynamicLogic.getAppDynamicLogic with the appropriate mocId and time).
evaluateRules ::
  ( BeamFlow m r,
    MonadFlow m,
    ClickhouseFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasYudhishthiraTablesSchema
  ) =>
  LYDL.CallerApp ->
  Id LYT.MerchantOperatingCity ->
  LYT.LogicDomain ->
  RuleFetcher m ->
  A.Value -> -- snapshot as JSON
  m OrchestratedOutput
evaluateRules callerApp mocId domain fetchRules snapshotJson = do
  (allLogics, _mbVersion) <- fetchRules domain
  if null allLogics
    then do
      logInfo $ "BehaviorEngine: No rules configured for domain " <> show domain <> ". Returning empty output."
      return defaultOrchestratedOutput
    else do
      logInfo $ "BehaviorEngine: Evaluating " <> show (length allLogics) <> " rules for domain " <> show domain
      result <-
        try @_ @SomeException $
          LYDL.runLogicsWithDebugLog callerApp mocId domain allLogics snapshotJson
      case result of
        Left err -> do
          logError $ "BehaviorEngine: Error evaluating rules for domain " <> show domain <> ": " <> show err
          return defaultOrchestratedOutput
        Right logicResp -> do
          logInfo $ "BehaviorEngine: Raw rule output for domain " <> show domain <> ": " <> show logicResp.result
          let (output, parseErrors) = extractOutput logicResp.result
          unless (null parseErrors) $
            logError $ "BehaviorEngine: Parse errors for domain " <> show domain <> ": " <> show parseErrors
          logInfo $
            "BehaviorEngine: Parsed output — Consequences: " <> show (length output.consequences)
              <> ", Communications: "
              <> show (length output.communications)
          return output

-- | Extract consequences and communications from rule engine JSON output
--
-- Expected format from rules:
-- {
--   "consequences": [{ "consequenceType": "HARD_BLOCK", "params": {...}, "requiresResolution": false }],
--   "communications": [{ "channel": "OVERLAY", "templateKey": "...", "params": {...}, "delaySeconds": 0 }]
-- }
extractOutput :: A.Value -> (OrchestratedOutput, [Text])
extractOutput val =
  case val of
    A.Object obj ->
      let (consequences, cErrs) = parseArrayField "consequences" obj
          (communications, cmErrs) = parseArrayField "communications" obj
       in (OrchestratedOutput {consequences, communications}, cErrs <> cmErrs)
    _ -> (defaultOrchestratedOutput, ["Rule output is not a JSON object: " <> show val])
  where
    parseArrayField :: (FromJSON a) => Text -> KM.KeyMap A.Value -> ([a], [Text])
    parseArrayField key obj =
      case KM.lookup (AK.fromText key) obj of
        Just v -> case A.fromJSON v of
          A.Success items -> (items, [])
          A.Error err -> ([], ["Failed to parse '" <> key <> "': " <> show err <> ". Raw value: " <> show v])
        Nothing -> ([], [])
