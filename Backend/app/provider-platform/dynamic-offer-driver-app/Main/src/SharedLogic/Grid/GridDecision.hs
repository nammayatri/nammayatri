module SharedLogic.Grid.GridDecision
  ( AiVerifyResult (..),
    aiVerifyDoc,
    isYes,
    callGridDecision,
    describeGridFailure,
  )
where

import qualified Control.Monad.Catch as Catch
import Data.Char (isAlphaNum, isAscii)
import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.AiVerificationQuestion as DAQ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Kernel.External.Grid.Error as GridError
import qualified Kernel.External.Grid.Flow as Grid
import qualified Kernel.External.Grid.Types as Grid
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Network.HTTP.Types (statusCode)
import qualified Servant.Client as Servant
import qualified Storage.CachedQueries.AiVerificationQuestion as CQAiQuestion
import Tools.Grid (getGridCfg)

-- | Grid's answers, keyed by the question label from the ai_verification_question table.
data AiVerifyResult = AiVerifyResult
  { answers :: Map.Map Grid.QuestionKey Grid.GridAnswer,
    -- | Yes-threshold of each NOUL question, used by 'isYes'.
    thresholds :: Map.Map Grid.QuestionKey Double
  }

-- | Generic AI verification of document images.
--
-- Loads the enabled questions configured for the given entity types (ai_verification_question):
-- the merchant-wide rows (no city) plus the city's own rows, a city row replacing a merchant-wide
-- row with the same label. It then fills each @{key}@ placeholder in the question text from the
-- given key-value pairs, asks Grid about the images, and returns the answers by question label.
-- What an answer means (approve, reject, tag) is left to the caller.
--
-- A question is skipped (with a warning) when its row is invalid or one of its placeholders has
-- no value, so a bad row never sends a half-filled question. Returns 'Nothing' when no question
-- could be built or the Grid call failed.
aiVerifyDoc ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, CoreMetrics m, HasRequestId r, MonadReader r m) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  -- | Entity types whose questions are asked, e.g. every document type in a batch of photos.
  NonEmpty Text ->
  -- | Placeholder values, e.g. [("rc_number", "KA01AB1234")]. Only pass trusted, verified data.
  [(Text, Text)] ->
  -- | Base64 images.
  NonEmpty Text ->
  m (Maybe AiVerifyResult)
aiVerifyDoc merchantId merchantOpCityId entityTypes placeholders rawImages = do
  rows <- concat <$> mapM (CQAiQuestion.findAllByMerchantIdAndEntityType merchantId) (toList entityTypes)
  let cityRows = [row | row <- rows, row.merchantOperatingCityId == Just merchantOpCityId]
      merchantRows = [row | row <- rows, isNothing row.merchantOperatingCityId]
      -- The same label may be configured under several entity types of a batch: the first one wins.
      -- A city row replaces a merchant-wide row with the same label, even when the city row is disabled.
      byLabel rs = Map.fromListWith (\_new old -> old) [(row.questionLabel, row) | row <- rs]
      questionRows = filter (.enabled) . Map.elems $ Map.union (byLabel cityRows) (byLabel merchantRows)
  built <- catMaybes <$> mapM buildQuestion questionRows
  case built of
    [] -> do
      logWarning $ "AI verification: no valid questions configured for " <> show (toList entityTypes)
      pure Nothing
    _ -> do
      let questions = Grid.mkGridQuestions [(key, q) | (key, q, _) <- built]
          thresholds = Map.fromList [(key, t) | (key, _, Just t) <- built]
      mbResp <- callGridDecision merchantId merchantOpCityId questions rawImages
      pure $ (\resp -> AiVerifyResult {answers = Grid.decodeAnswers questions resp, thresholds}) <$> mbResp
  where
    buildQuestion row = case mkGridQuestion placeholders row of
      Left reason -> do
        logWarning $ "AI verification: skipping question " <> row.questionLabel <> ": " <> reason
        pure Nothing
      Right (q, mbThreshold) -> pure $ Just (Grid.QuestionKey row.questionLabel, q, mbThreshold)

-- | Whether a NOUL question was answered yes, using the threshold from its row (default 0.5).
-- 'Nothing' if the question has no valid answer.
isYes :: Grid.QuestionKey -> AiVerifyResult -> Maybe Bool
isYes key result = do
  answer <- Grid.getNoul key result.answers
  pure $ answer.pYes >= Map.findWithDefault defaultThreshold key result.thresholds

defaultThreshold :: Double
defaultThreshold = 0.5

mkGridQuestion :: [(Text, Text)] -> DAQ.AiVerificationQuestion -> Either Text (Grid.GridQuestion, Maybe Double)
mkGridQuestion placeholders row = do
  instructions <- fillPlaceholders placeholders row.question
  case row.questionType of
    DAQ.NOUL -> do
      threshold <- case row.threshold of
        Nothing -> Right defaultThreshold
        Just t
          | t >= 0 && t <= 1 -> Right t
          | otherwise -> Left "threshold must be between 0 and 1"
      Right (Grid.NoulQ instructions, Just threshold)
    DAQ.CHOICE -> do
      options <- requireOptions
      filled <- forM options $ \o -> (o.key,) <$> fillPlaceholders placeholders o.text
      Right (Grid.ChoiceQ instructions filled, Nothing)
    DAQ.SCORE -> do
      options <- requireOptions
      levels <- forM options $ \o -> fillPlaceholders placeholders o.text
      Right (Grid.ScoreQ instructions levels, Nothing)
  where
    requireOptions = maybe (Left "options are required for CHOICE and SCORE questions") Right (row.options >>= NE.nonEmpty)

-- | Replaces every @{key}@ with its sanitised value. Fails if a placeholder is left without a value,
-- so Grid never receives literal placeholder text.
fillPlaceholders :: [(Text, Text)] -> Text -> Either Text Text
fillPlaceholders placeholders text =
  let filled = foldl' (\acc (key, value) -> T.replace ("{" <> key <> "}") (sanitizeValue value) acc) text placeholders
   in maybe (Right filled) (\missing -> Left $ "no value for placeholder {" <> missing <> "}") (findPlaceholder filled)

-- | Values end up in the prompt, so only plain characters are kept (no braces, quotes or newlines),
-- which stops a value from injecting instructions or new placeholders.
sanitizeValue :: Text -> Text
sanitizeValue = T.take 64 . T.strip . T.filter (\c -> isAscii c && (isAlphaNum c || c `elem` (" -./" :: String)))

findPlaceholder :: Text -> Maybe Text
findPlaceholder text = case T.breakOn "{" text of
  (_, rest)
    | T.null rest -> Nothing
    | otherwise ->
      let afterBrace = T.drop 1 rest
          (name, afterName) = T.span isKeyChar afterBrace
       in if not (T.null name) && "}" `T.isPrefixOf` afterName then Just name else findPlaceholder afterBrace
  where
    isKeyChar c = isAscii c && (isAlphaNum c || c == '_')

callGridDecision ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, CoreMetrics m, HasRequestId r, MonadReader r m) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Grid.GridQuestions ->
  NonEmpty Text ->
  m (Maybe Grid.GridDecisionResp)
callGridDecision merchantId merchantOpCityId questions rawImages = do
  result <- Catch.try @_ @SomeException runCheck
  case result of
    Left err
      | Just (asyncErr :: SomeAsyncException) <- fromException err -> Catch.throwM asyncErr
      | otherwise -> do
        logWarning $ "Grid decision call failed: " <> describeGridFailure err
        pure Nothing
    Right mbResp -> pure mbResp
  where
    runCheck =
      getGridCfg merchantId merchantOpCityId >>= \case
        Nothing -> pure Nothing
        Just gridCfg -> do
          let (rejected, accepted) = partitionEithers (Grid.mkGridImageFromBase64 gridCfg.maxImageBytes <$> toList rawImages)
          unless (null rejected) $
            logWarning $ "Grid decision: dropped " <> show (length rejected) <> " image(s) that could not be sent: " <> T.intercalate "; " rejected
          case NE.nonEmpty accepted of
            Nothing -> pure Nothing
            Just images ->
              Just
                <$> Grid.decide
                  gridCfg
                  Grid.GridDecisionReq
                    { model = gridCfg.model,
                      -- Grid requires the field; all context lives in the question text.
                      state = "",
                      images = toList images,
                      questions
                    }

describeGridFailure :: SomeException -> Text
describeGridFailure err
  | Just (gridErr :: GridError.GridError) <- fromException err = show gridErr
  | Just (apiErr :: ExternalAPICallError) <- fromException err = case apiErr.clientError of
    Servant.FailureResponse _ resp -> "HTTP status " <> show (statusCode (Servant.responseStatusCode resp))
    _ -> "request error (detail withheld)"
  | otherwise = "unexpected error (detail withheld)"
