-- | Which questions a rider gets: the platform's targeting logic (app_dynamic_logic_element, one
-- domain per kind of feedback) run on its context. Fails closed: no logic, unsupported operators, an
-- error or unparsable output all select nothing.
module Lib.Feedback.Selection
  ( FeedbackSelection (..),
    QuestionSelection (..),
    selectQuestions,
    withoutTxnStickiness,
    rolloutToss,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified EulerHS.Language as L
import Kernel.Beam.Types (TxnIdKey (..))
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CHConfig
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Feedback.Rule (stableBucket, unsupportedOperators)
import Lib.Yudhishthira.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified Tools.DynamicLogic as TDL

-- | Output of a feedback targeting logic: the keys of the questions to ask. Content, timing and limits
-- of each key come from the platform's question table.
newtype FeedbackSelection = FeedbackSelection
  { questions :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QuestionSelection = QuestionSelection
  { questionKeys :: [Text],
    -- | Version picked by app_dynamic_logic_rollout; Nothing when the city has no logic.
    logicVersion :: Maybe Int
  }

-- | Runs the city's logic of the domain on the context. The rollout version is chosen with a toss
-- derived from 'tossKey' (the rider), so a rider stays in the same A/B group on every fetch.
selectQuestions ::
  (BeamFlow m r, CHConfig.ClickhouseFlow m r, ToJSON ctx) =>
  LYDL.CallerApp ->
  LYT.LogicDomain ->
  Id LYT.MerchantOperatingCity ->
  -- | Stable A/B key: the rider's id.
  Text ->
  UTCTime ->
  ctx ->
  m QuestionSelection
selectQuestions callerApp domain merchantOpCityId tossKey localTime ctx = do
  (logics, mbVersion) <- withoutTxnStickiness $ TDL.getAppDynamicLogic merchantOpCityId domain localTime Nothing (Just $ rolloutToss tossKey)
  let noQuestions = QuestionSelection {questionKeys = [], logicVersion = mbVersion}
      tag = show domain <> " logic version " <> show mbVersion
  case concatMap outputOperators logics of
    _ | null logics -> pure noQuestions
    ops@(_ : _) -> do
      logError $ "Feedback: " <> tag <> " uses unsupported operators: " <> T.intercalate ", " ops
      pure noQuestions
    [] -> do
      res <- withTryCatch ("runLogics:" <> show domain) $ LYDL.runLogicsWithDebugLog callerApp merchantOpCityId domain Nothing logics ctx
      case res of
        Left err -> do
          logError $ "Feedback: " <> tag <> " failed: " <> show err
          pure noQuestions
        Right resp -> case A.fromJSON resp.result of
          A.Success (selection :: FeedbackSelection) -> pure QuestionSelection {questionKeys = selection.questions, logicVersion = mbVersion}
          A.Error err -> do
            logError $ "Feedback: " <> tag <> " returned an unexpected result (" <> T.pack err <> "): " <> show resp.result
            pure noQuestions

-- | The rider toss already keeps a rider in one rollout group, so the version is not also pinned
-- to the request's transaction: with the pin, a saved rule would not reach journeys in progress for
-- two hours. The transaction id (set for Config Pilot) is put back afterwards, also when the action throws.
withoutTxnStickiness :: (L.MonadFlow m, MonadMask m) => m a -> m a
withoutTxnStickiness action = do
  mbTxnId <- L.getOptionLocal TxnIdKey
  L.delOptionLocal TxnIdKey
  action `finally` whenJust mbTxnId (L.setOptionLocal TxnIdKey)

-- | Rollout toss in [1, 100], stable per key (percentage splits compare against it).
rolloutToss :: Text -> Int
rolloutToss tossKey = stableBucket tossKey + 1

-- | Operators used by a logic of the form {"questions": <rule>}. The output key itself is a plain
-- object key, not an operator, so only the rule under it is checked.
outputOperators :: A.Value -> [Text]
outputOperators logic = case logic of
  A.Object obj | [("questions", rule)] <- KM.toList obj -> unsupportedOperators rule
  _ -> unsupportedOperators logic
