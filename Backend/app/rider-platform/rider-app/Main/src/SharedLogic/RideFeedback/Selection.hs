module SharedLogic.RideFeedback.Selection
  ( RideFeedbackSelection (..),
    QuestionSelection (..),
    selectQuestions,
    selectedLogicVersion,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.RideFeedback.Context (RideFeedbackContext)
import SharedLogic.RideFeedback.Rule (stableBucket, unsupportedOperators)
import Storage.Beam.Yudhishthira ()
import qualified Tools.DynamicLogic as TDL

-- | Output of the RIDE-FEEDBACK logic (app_dynamic_logic_element): the keys of the questions this
-- ride should get. Content, timing and limits of each key come from ride_feedback_config.
newtype RideFeedbackSelection = RideFeedbackSelection
  { questions :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QuestionSelection = QuestionSelection
  { questionKeys :: [Text],
    -- | Version picked by app_dynamic_logic_rollout; Nothing when the city has no logic.
    logicVersion :: Maybe Int
  }

-- | Runs the city's RIDE-FEEDBACK logic on the ride context. The rollout version is chosen with a
-- toss derived from the rider id, so a rider stays in the same A/B group on every poll and ride.
-- No logic, unsupported operators, an error or unparsable output all select nothing (fail closed).
selectQuestions ::
  Id DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  UTCTime ->
  RideFeedbackContext ->
  Flow QuestionSelection
selectQuestions merchantOpCityId personId localTime ctx = do
  (logics, mbVersion) <- TDL.getAppDynamicLogic (cast merchantOpCityId) LYT.RIDE_FEEDBACK localTime Nothing (Just $ riderToss personId)
  let noQuestions = QuestionSelection {questionKeys = [], logicVersion = mbVersion}
  case concatMap outputOperators logics of
    _ | null logics -> pure noQuestions
    ops@(_ : _) -> do
      logError $ "RideFeedback: RIDE-FEEDBACK logic version " <> show mbVersion <> " uses unsupported operators: " <> T.intercalate ", " ops
      pure noQuestions
    [] -> do
      res <- withTryCatch "runLogics:RIDE_FEEDBACK" $ LYDL.runLogicsWithDebugLog LYDL.Rider (cast merchantOpCityId) LYT.RIDE_FEEDBACK Nothing logics ctx
      case res of
        Left err -> do
          logError $ "RideFeedback: RIDE-FEEDBACK logic version " <> show mbVersion <> " failed: " <> show err
          pure noQuestions
        Right resp -> case A.fromJSON resp.result of
          A.Success (selection :: RideFeedbackSelection) -> pure QuestionSelection {questionKeys = selection.questions, logicVersion = mbVersion}
          A.Error err -> do
            logError $ "RideFeedback: RIDE-FEEDBACK logic version " <> show mbVersion <> " returned an unexpected result (" <> T.pack err <> "): " <> show resp.result
            pure noQuestions

-- | The version 'selectQuestions' picks for this rider, without running the logic (used when saving answers).
selectedLogicVersion :: Id DMOC.MerchantOperatingCity -> Id DP.Person -> UTCTime -> Flow (Maybe Int)
selectedLogicVersion merchantOpCityId personId localTime =
  TDL.selectAppDynamicLogicVersion (cast merchantOpCityId) LYT.RIDE_FEEDBACK localTime (Just $ riderToss personId)

-- | Rollout toss in [1, 100], stable per rider (percentage splits compare against it).
riderToss :: Id DP.Person -> Int
riderToss personId = stableBucket personId.getId + 1

-- | Operators used by a logic of the form {"questions": <rule>}. The output key itself is a plain
-- object key, not an operator, so only the rule under it is checked.
outputOperators :: A.Value -> [Text]
outputOperators logic = case logic of
  A.Object obj | [("questions", rule)] <- KM.toList obj -> unsupportedOperators rule
  _ -> unsupportedOperators logic
