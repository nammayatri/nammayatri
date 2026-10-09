module SharedLogic.RideFeedback.Selection
  ( RideFeedbackSelection (..),
    QuestionSelection (..),
    selectQuestions,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment
import qualified EulerHS.Language as L
import Kernel.Beam.Types (TxnIdKey (..))
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
-- toss derived from the rider, so a rider stays in the same A/B group on every fetch and ride.
-- No logic, unsupported operators, an error or unparsable output all select nothing (fail closed).
selectQuestions ::
  Id DMOC.MerchantOperatingCity ->
  -- | Stable A/B key: the rider's BPP id, or the ride id when the rider is unknown.
  Text ->
  UTCTime ->
  RideFeedbackContext ->
  Flow QuestionSelection
selectQuestions merchantOpCityId tossKey localTime ctx = do
  (logics, mbVersion) <- withoutTxnStickiness $ TDL.getAppDynamicLogic (cast merchantOpCityId) LYT.RIDE_FEEDBACK localTime Nothing (Just $ rolloutToss tossKey)
  let noQuestions = QuestionSelection {questionKeys = [], logicVersion = mbVersion}
  case concatMap outputOperators logics of
    _ | null logics -> pure noQuestions
    ops@(_ : _) -> do
      logError $ "RideFeedback: RIDE-FEEDBACK logic version " <> show mbVersion <> " uses unsupported operators: " <> T.intercalate ", " ops
      pure noQuestions
    [] -> do
      res <- withTryCatch "runLogics:RIDE_FEEDBACK" $ LYDL.runLogicsWithDebugLog LYDL.Driver (cast merchantOpCityId) LYT.RIDE_FEEDBACK Nothing logics ctx
      case res of
        Left err -> do
          logError $ "RideFeedback: RIDE-FEEDBACK logic version " <> show mbVersion <> " failed: " <> show err
          pure noQuestions
        Right resp -> case A.fromJSON resp.result of
          A.Success (selection :: RideFeedbackSelection) -> pure QuestionSelection {questionKeys = selection.questions, logicVersion = mbVersion}
          A.Error err -> do
            logError $ "RideFeedback: RIDE-FEEDBACK logic version " <> show mbVersion <> " returned an unexpected result (" <> T.pack err <> "): " <> show resp.result
            pure noQuestions

-- | The rider toss already keeps a rider in one rollout group, so the version is not also pinned
-- to the ride's transaction: with the pin, a saved rule would not reach rides in progress for
-- two hours. The transaction id (set for Config Pilot) is put back afterwards.
withoutTxnStickiness :: Flow a -> Flow a
withoutTxnStickiness action = do
  mbTxnId <- L.getOptionLocal TxnIdKey
  L.delOptionLocal TxnIdKey
  res <- action
  whenJust mbTxnId (L.setOptionLocal TxnIdKey)
  pure res

-- | Rollout toss in [1, 100], stable per key (percentage splits compare against it).
rolloutToss :: Text -> Int
rolloutToss tossKey = stableBucket tossKey + 1

-- | Operators used by a logic of the form {"questions": <rule>}. The output key itself is a plain
-- object key, not an operator, so only the rule under it is checked.
outputOperators :: A.Value -> [Text]
outputOperators logic = case logic of
  A.Object obj | [("questions", rule)] <- KM.toList obj -> unsupportedOperators rule
  _ -> unsupportedOperators logic
