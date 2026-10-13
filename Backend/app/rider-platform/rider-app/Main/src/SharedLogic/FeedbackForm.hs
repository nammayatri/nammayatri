{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FeedbackForm
  ( FeedbackFormLogicInput (..),
    FeedbackFormLogicOutput (..),
    defaultFeedbackFormLogicInput,
    getFeedbackFormLogics,
    runFeedbackFormLogic,
  )
where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.ServiceTierType (ServiceTierType (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Tools.DynamicLogic as DynamicLogic

-- | What the FEEDBACK_FORM logic sees, once per rating group.
-- `questions` (question keys, in display order) and `badges` (question key -> pill keys,
-- in display order) start with only the shouldDefault items, i.e. the form a rider gets
-- without any logic. A rule changes them in place:
--   add      {"cat":[{"var":""},{"questions":["Q"],"badges":{"Q":["P"]}}]}  (cat appends arrays)
--   remove   {"filter":[{"var":"badges.Q"},{"!":[{"in":[{"var":""},["P"]]}]}]}
--   replace  {"replaceMerge":[{"var":""},{"questions":[...],"badges":{...}}]}
--   no-op    {"var":""} or null
data FeedbackFormLogicInput = FeedbackFormLogicInput
  { rating :: Maybe Int,
    serviceTier :: ServiceTierType,
    isAirConditioned :: Bool,
    specialLocationName :: Maybe Text,
    questions :: [Text],
    badges :: HM.HashMap Text [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Sample input the dashboard uses to dry-run a logic and to show the domain schema.
defaultFeedbackFormLogicInput :: FeedbackFormLogicInput
defaultFeedbackFormLogicInput =
  FeedbackFormLogicInput
    { rating = Nothing,
      serviceTier = SEDAN,
      isAirConditioned = False,
      specialLocationName = Nothing,
      questions = [],
      badges = HM.empty
    }

-- | Read back from the logic result. Every key is resolved against the feedback_form
-- catalogue by the caller; unknown keys are dropped. A question listed in `questions`
-- but missing from `badges` shows its own shouldDefault pills.
data FeedbackFormLogicOutput = FeedbackFormLogicOutput
  { questions :: [Text],
    badges :: Maybe (HM.HashMap Text [Text])
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | The FEEDBACK_FORM logic rolled out for the city; [] when none is configured.
-- Loaded once per request so it can be run for each rating group.
getFeedbackFormLogics ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id DMOC.MerchantOperatingCity ->
  m [A.Value]
getFeedbackFormLogics merchantOpCityId = do
  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
  let timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig
  localTime <- getLocalCurrentTime timeDiffFromUtc
  (allLogics, _mbVersion) <-
    DynamicLogic.getAppDynamicLogic
      (cast merchantOpCityId)
      LYT.FEEDBACK_FORM
      localTime
      Nothing
      Nothing
  when (null allLogics) $
    logTagInfo ("MOC-" <> merchantOpCityId.getId) "No FeedbackForm logic configured; showing the default form."
  return allLogics

-- | Nothing => no logic, the logic returned null (an explicit "keep the defaults"),
-- or it failed; the caller then shows the default (shouldDefault) form.
runFeedbackFormLogic ::
  (MonadFlow m) =>
  [A.Value] ->
  FeedbackFormLogicInput ->
  m (Maybe FeedbackFormLogicOutput)
runFeedbackFormLogic [] _ = return Nothing
runFeedbackFormLogic allLogics inputData = do
  response <- withTryCatch "runLogics:FeedbackForm" $ LYTU.runLogics allLogics inputData
  case response of
    Left e -> do
      logError $ "Error in running FeedbackForm logics - " <> show e
      return Nothing
    Right resp -> case resp.result of
      A.Null -> do
        logDebug $ "FeedbackForm logic returned null for rating " <> show inputData.rating <> "; keeping the default form."
        return Nothing
      result ->
        case A.fromJSON @FeedbackFormLogicOutput result of
          A.Success output -> do
            logDebug $ "FeedbackForm logic result for rating " <> show inputData.rating <> ": " <> show output
            return $ Just output
          A.Error err -> do
            logWarning $ "Error parsing FeedbackForm logic result - " <> show err <> " - " <> show result
            return Nothing
