-- | What every feedback question has, whatever asks it (ride feedback on the driver platform, app
-- feedback on the rider platform): config checks, answer checks and scoring, follow-ups, priority and
-- action-rule matching. Written over the fields both question tables share ('FeedbackQuestion'), so
-- each platform keeps its own table, ids and placement (ride stages / app placements).
module Lib.Feedback.Question
  ( FeedbackQuestion,
    QuestionRules (..),
    validateQuestion,
    validateAnswer,
    answerScore,
    usesOptions,
    collectFollowUps,
    isFollowUpOnly,
    priorityKey,
    defaultPriority,
    matchActionRules,
    pendingResult,
    sameAction,
    pickTranslation,
    reportableIssueTypes,
    validIconUrl,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAsciiUpper, isDigit)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (nub, (\\))
import qualified Data.Text as T
import IssueManagement.Common (IssueReportType (..), Translation (..))
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import Lib.Feedback.Rule (evaluateRule, unsupportedOperators)
import Lib.Feedback.Types

-- | The fields of a question config both question tables have.
type FeedbackQuestion q =
  ( HasField "questionKey" q Text,
    HasField "questionType" q FeedbackQuestionType,
    HasField "title" q [Translation],
    HasField "description" q (Maybe [Translation]),
    HasField "acknowledgement" q (Maybe [Translation]),
    HasField "iconUrl" q (Maybe Text),
    HasField "options" q (Maybe [QuestionOption]),
    HasField "inputConfig" q (Maybe InputConfig),
    HasField "uiConfig" q (Maybe A.Value),
    HasField "isFollowUpOnly" q (Maybe Bool),
    HasField "priority" q (Maybe Int),
    HasField "actionRules" q (Maybe [FeedbackActionRule])
  )

-- | What a platform adds to the shared checks.
data QuestionRules q = QuestionRules
  { -- | Problems with the platform's own fields (stages, placements, timing), listed after the content's.
    ownErrors :: q -> [Text],
    -- | Problems with an option of the first question opening the second (e.g. asked at a different level).
    followUpErrors :: q -> q -> [Text],
    -- | Why an action type cannot be used on this question (completes "action X in rule R ..."), if it cannot.
    actionTypeError :: q -> FeedbackActionType -> Maybe Text
  }

-- | Human-readable problems with a question config; empty means valid. 'cityQuestions' are the other
-- questions of the same city (to resolve option.nextQuestionKey); the question itself may be among them.
validateQuestion :: FeedbackQuestion q => QuestionRules q -> [q] -> q -> [Text]
validateQuestion QuestionRules {ownErrors, followUpErrors, actionTypeError} cityQuestions c =
  concat
    [ check (validKey c.questionKey) "questionKey must be 2-64 characters of A-Z, 0-9 and _, starting with a letter",
      translations "title" (Just c.title),
      translations "description" c.description,
      translations "acknowledgement" c.acknowledgement,
      check (maybe True validIconUrl c.iconUrl) "iconUrl must be an https:// URL",
      optionErrors,
      inputConfigErrors,
      ownErrors c,
      check (maybe True (>= 0) c.priority) "priority must not be negative",
      check (maybe True isObject c.uiConfig) "uiConfig must be a JSON object",
      actionRuleErrors
    ]
  where
    check cond msg = [msg | not cond]
    isObject = \case A.Object _ -> True; _ -> False

    translations field = \case
      Nothing -> []
      Just ts ->
        check (any (\t -> t.language == ENGLISH) ts) (field <> " needs an ENGLISH translation")
          <> check (not (any (T.null . T.strip . (.translation)) ts)) (field <> " has an empty translation")

    options = fromMaybe [] c.options
    optionKeys = map (.key) options
    others = [x | x <- cityQuestions, x.questionKey /= c.questionKey]
    optionErrors
      | usesOptions c.questionType =
        check (not (null options)) (show c.questionType <> " needs at least one option")
          <> check (length (nub optionKeys) == length optionKeys) "option keys must be unique"
          <> check (all validKey optionKeys) "option keys must be 2-64 characters of A-Z, 0-9 and _, starting with a letter"
          <> check (all (maybe True (\x -> x >= 0 && x <= 1) . (.score)) options) "option scores must be between 0 and 1"
          <> concat [check (validIconUrl url) ("option " <> o.key <> " iconUrl must be an https:// URL") | o <- options, Just url <- [o.iconUrl]]
          <> concatMap (\o -> translations ("option " <> o.key <> " label") (Just o.label)) options
          <> concat
            [ check (next /= c.questionKey) ("option " <> o.key <> " cannot open its own question")
                <> case find ((== next) . (.questionKey)) others of
                  Nothing -> ["option " <> o.key <> " opens " <> next <> ", which does not exist in this city"]
                  Just target -> map (("option " <> o.key <> " opens " <> next <> ", ") <>) (followUpErrors c target)
              | o <- options,
                Just next <- [o.nextQuestionKey]
            ]
      | otherwise = check (null options) (show c.questionType <> " does not take options")

    inputConfigErrors = case c.inputConfig of
      Nothing -> []
      Just ic ->
        ordered "minLength" ic.minLength "maxLength" ic.maxLength
          <> ordered "minValue" ic.minValue "maxValue" ic.maxValue
          <> ordered "minSelections" ic.minSelections "maxSelections" ic.maxSelections
          <> check (all (>= 0) (catMaybes [ic.minLength, ic.maxLength, ic.minSelections, ic.maxSelections])) "inputConfig lengths and selections must not be negative"
          <> check (maybe True (\n -> n >= 1 && n <= 10) ic.maxFiles) "inputConfig.maxFiles must be between 1 and 10"
          <> check (maybe True (\n -> n >= 1 && n <= 300) ic.maxDurationSec) "inputConfig.maxDurationSec must be between 1 and 300"
          <> check (maybe True (> 0) ic.step) "inputConfig.step must be positive"
          <> translations "inputConfig.placeholder" ic.placeholder
    ordered :: Ord a => Text -> Maybe a -> Text -> Maybe a -> [Text]
    ordered lo mbLo hi mbHi = check (fromMaybe True ((<=) <$> mbLo <*> mbHi)) ("inputConfig." <> lo <> " must not exceed " <> hi)

    actionRules = fromMaybe [] c.actionRules
    ruleIds = map (.ruleId) actionRules
    actionRuleErrors =
      check (not (any (T.null . T.strip) ruleIds)) "every action rule needs a ruleId"
        <> check (null (ruleIds \\ nub ruleIds)) "action rule ids must be unique"
        <> concatMap ruleErrors actionRules
    ruleErrors rule =
      [ "action rule " <> rule.ruleId <> " uses unsupported operators: " <> T.intercalate ", " ops
        | Just cond <- [rule.condition],
          let ops = unsupportedOperators cond,
          not (null ops)
      ]
        <> check (not (null rule.actions)) ("action rule " <> rule.ruleId <> " has no actions")
        <> concatMap (actionErrors rule.ruleId) rule.actions
    actionErrors ruleId action =
      [ "action " <> show action.actionType <> " in rule " <> ruleId <> " " <> reason
        | Just reason <- [actionTypeError c action.actionType]
      ]
        <> actionParamErrors ruleId action
    actionParamErrors ruleId action =
      let needs key = check (hasTextParam key action.params) ("action " <> show action.actionType <> " in rule " <> ruleId <> " needs param " <> key)
       in case action.actionType of
            REPORT_ISSUE_TO_BPP ->
              check
                (maybe False (`elem` reportableIssueTypes) (textParam "issueType" action.params >>= parseIssueType))
                ("action REPORT_ISSUE_TO_BPP in rule " <> ruleId <> " needs param issueType, one of " <> T.intercalate ", " (map show reportableIssueTypes))
            CREATE_TICKET -> needs "category"
            NOTIFY_RIDER -> needs "notificationKey"
            _ -> []

-- | Returns the answer to store, or an error code for the client. Only the parts of an answer that belong
-- to the question's type are accepted (anything else is rejected, not dropped, so a client bug shows
-- up), and option keys must be the question's own.
validateAnswer :: FeedbackQuestion q => q -> Maybe FeedbackAnswer -> Either Text FeedbackAnswer
validateAnswer config = \case
  Nothing -> Left "ANSWER_REQUIRED"
  Just ans -> do
    let selectedKeys = nub $ fromMaybe [] ans.selectedOptionKeys
        options = fromMaybe [] config.options
        optionKeys = map (.key) options
        picked = [o | o <- options, o.key `elem` selectedKeys]
        exclusiveKeys = [o.key | o <- options, o.isExclusive == Just True]
        ic = config.inputConfig
        inputInt f = ic >>= f
        inputDouble f = ic >>= f
        text = mfilter (not . T.null) (T.strip <$> ans.text)
        textLen = maybe 0 T.length text
        files = fromMaybe [] ans.mediaFileIds
        withinD lo hi v = maybe True (v >=) lo && maybe True (v <=) hi
        -- A comment is part of a choice answer only when a picked option asks for one.
        commentWanted = any ((== Just True) . (.requiresText)) picked
        -- The parts of an answer (options, text, number, rating, media) the question's type takes.
        only :: [Text] -> Either Text ()
        only fields =
          check
            ( and
                [ "options" `elem` fields || null selectedKeys,
                  "text" `elem` fields || isNothing text,
                  "number" `elem` fields || isNothing ans.number,
                  "rating" `elem` fields || isNothing ans.rating,
                  "media" `elem` fields || null files
                ]
            )
            "UNEXPECTED_FIELD"
        onStep v = maybe True (\st -> let n = (v - fromMaybe 0 (inputDouble (.minValue))) / st in abs (n - fromIntegral (round n :: Int)) < 1e-9) (inputDouble (.step))
        choice = do
          check (all (`elem` optionKeys) selectedKeys) "INVALID_OPTION"
          only (["options"] <> ["text" | commentWanted])
          when commentWanted $ check (textLen > 0) "COMMENT_REQUIRED"
    check (textLen <= fromMaybe defaultMaxTextLength (inputInt (.maxLength))) "TEXT_TOO_LONG"
    case config.questionType of
      t
        | t `elem` [SINGLE_SELECT, YES_NO, THUMBS, EMOJI_SCALE] -> do
          check (length selectedKeys == 1) "INVALID_OPTION"
          choice
      MULTI_SELECT -> do
        check (not (null selectedKeys)) "INVALID_OPTION"
        choice
        check (length selectedKeys >= fromMaybe 1 (inputInt (.minSelections)) && maybe True (length selectedKeys <=) (inputInt (.maxSelections))) "INVALID_SELECTION_COUNT"
        check (not (any (`elem` exclusiveKeys) selectedKeys) || length selectedKeys == 1) "EXCLUSIVE_OPTION_COMBINED"
      STAR_RATING -> do
        only ["rating"]
        check (maybe False (withinD (Just $ fromMaybe 1 (inputDouble (.minValue))) (Just $ fromMaybe 5 (inputDouble (.maxValue))) . fromIntegral) ans.rating) "INVALID_RATING"
      t
        | t `elem` [SCALE, NUMBER_INPUT] -> do
          only ["number"]
          check (maybe False (\v -> withinD (inputDouble (.minValue)) (inputDouble (.maxValue)) v && onStep v) ans.number) "INVALID_NUMBER"
      TEXT_INPUT -> do
        only ["text"]
        check (textLen > 0 && textLen >= fromMaybe 0 (inputInt (.minLength))) "INVALID_TEXT"
      t
        | t `elem` [IMAGE_UPLOAD, AUDIO] -> do
          only ["media"]
          check (not (null files) && length files <= fromMaybe defaultMaxFiles (inputInt (.maxFiles))) "INVALID_MEDIA"
      _ -> pure ()
    pure
      FeedbackAnswer
        { selectedOptionKeys = if null selectedKeys then Nothing else Just selectedKeys,
          text,
          number = ans.number,
          rating = ans.rating,
          mediaFileIds = if null files then Nothing else Just files
        }
  where
    check cond code = unless cond (Left code)

-- | 0 (bad) to 1 (good), comparable across questions, so answers can be aggregated without knowing what
-- each question means: a rating or number placed within the question's range, or the configured score
-- of the picked option (the lowest one for a multi-select). Nothing when the question is not scored
-- (text, media, options without scores, a number without both bounds).
answerScore :: FeedbackQuestion q => q -> FeedbackAnswer -> Maybe Double
answerScore config ans = case config.questionType of
  STAR_RATING -> placed (Just $ fromMaybe 1 (ic >>= (.minValue))) (Just $ fromMaybe 5 (ic >>= (.maxValue))) (fromIntegral <$> ans.rating)
  SCALE -> placed (ic >>= (.minValue)) (ic >>= (.maxValue)) ans.number
  NUMBER_INPUT -> placed (ic >>= (.minValue)) (ic >>= (.maxValue)) ans.number
  _ ->
    let picked = fromMaybe [] ans.selectedOptionKeys
        scores = [s | o <- fromMaybe [] config.options, o.key `elem` picked, Just s <- [o.score]]
     in if null scores then Nothing else Just (minimum scores)
  where
    ic = config.inputConfig
    placed mbLo mbHi mbV = do
      lo <- mbLo
      hi <- mbHi
      v <- mbV
      guard (hi > lo)
      pure $ max 0 (min 1 ((v - lo) / (hi - lo)))

-- | Question types answered by picking option keys (they need 'options'). Exhaustive, so a new
-- question type does not compile until it is placed here.
usesOptions :: FeedbackQuestionType -> Bool
usesOptions = \case
  SINGLE_SELECT -> True
  MULTI_SELECT -> True
  YES_NO -> True
  THUMBS -> True
  EMOJI_SCALE -> True
  STAR_RATING -> False
  SCALE -> False
  TEXT_INPUT -> False
  NUMBER_INPUT -> False
  IMAGE_UPLOAD -> False
  AUDIO -> False

-- | Follow-ups reachable through option.nextQuestionKey, breadth-first, at most 3 levels deep.
-- They are not in the targeting logic's output: a follow-up is shown only through the parent it selected.
collectFollowUps :: FeedbackQuestion q => HM.HashMap Text q -> (q -> Bool) -> [q] -> [q]
collectFollowUps byKey usable roots = go (3 :: Int) (HS.fromList $ map (.questionKey) roots) roots
  where
    go 0 _ _ = []
    go depth seen parents =
      let nextKeys = nub [k | p <- parents, o <- fromMaybe [] p.options, Just k <- [o.nextQuestionKey], not (HS.member k seen)]
          level = filter usable $ mapMaybe (`HM.lookup` byKey) nextKeys
       in if null level then [] else level <> go (depth - 1) (HS.union seen (HS.fromList nextKeys)) level

isFollowUpOnly :: FeedbackQuestion q => q -> Bool
isFollowUpOnly = fromMaybe False . (.isFollowUpOnly)

priorityKey :: FeedbackQuestion q => q -> (Int, Text)
priorityKey c = (fromMaybe defaultPriority c.priority, c.questionKey)

defaultPriority :: Int
defaultPriority = 100

-- | Actions of every rule whose condition matches, in rule order, and an error per rule that failed to
-- evaluate (skipped, never matched by accident; the caller logs them).
matchActionRules :: (FeedbackQuestion q, ToJSON ctx) => q -> ctx -> ([MatchedFeedbackAction], [Text])
matchActionRules config ctx = foldr step ([], []) (fromMaybe [] config.actionRules)
  where
    step rule (matched, errors) = case evaluateRule rule.condition ctx of
      Right True -> ([MatchedFeedbackAction {ruleId = rule.ruleId, actionType = a.actionType, params = a.params} | a <- rule.actions] <> matched, errors)
      Right False -> (matched, errors)
      Left err -> (matched, ("action rule " <> rule.ruleId <> " of " <> config.questionKey <> ": " <> err) : errors)

-- | How a matched action is recorded before it runs, so a lost run still leaves a trail to retry from.
pendingResult :: UTCTime -> MatchedFeedbackAction -> FeedbackActionResult
pendingResult now action =
  FeedbackActionResult
    { ruleId = action.ruleId,
      actionType = action.actionType,
      status = PENDING,
      attempts = 0,
      externalRef = Nothing,
      errorMessage = Nothing,
      updatedAt = now
    }

-- | Same rule and action type: one entry of a response's action results.
sameAction :: FeedbackActionResult -> FeedbackActionResult -> Bool
sameAction a b = a.ruleId == b.ruleId && a.actionType == b.actionType

-- | The language asked for, else English, else the first translation.
pickTranslation :: Language -> [Translation] -> Maybe Text
pickTranslation language translations =
  (.translation)
    <$> ( find ((== language) . (.language)) translations
            <|> find ((== ENGLISH) . (.language)) translations
            <|> listToMaybe translations
        )

-- | Issue types the driver platform's /internal/{rideId}/reportIssue acts on (the dashboard offers these).
reportableIssueTypes :: [IssueReportType]
reportableIssueTypes = filter isReportableIssue [minBound .. maxBound]

-- | Exhaustive, so a new issue type does not compile until it is decided here.
isReportableIssue :: IssueReportType -> Bool
isReportableIssue = \case
  AC_RELATED_ISSUE -> True
  DRIVER_TOLL_RELATED_ISSUE -> True
  EXTRA_FARE_MITIGATION -> True
  DRUNK_AND_DRIVE_VIOLATION -> True
  UNHYGIENIC_VEHICLE -> True
  VEHICLE_UNSAFE -> True
  SYNC_BOOKING -> False

-- | Images are loaded by the rider app, which only fetches https.
validIconUrl :: Text -> Bool
validIconUrl url = "https://" `T.isPrefixOf` url && T.length url > 8 && not (T.any (== ' ') url)

validKey :: Text -> Bool
validKey k =
  T.length k >= 2 && T.length k <= 64
    && maybe False (isAsciiUpper . fst) (T.uncons k)
    && T.all (\ch -> isAsciiUpper ch || isDigit ch || ch == '_') k

textParam :: Text -> Maybe A.Value -> Maybe Text
textParam key = \case
  Just (A.Object obj) -> case KM.lookup (AK.fromText key) obj of
    Just (A.String v) | not (T.null (T.strip v)) -> Just v
    _ -> Nothing
  _ -> Nothing

parseIssueType :: Text -> Maybe IssueReportType
parseIssueType v = case A.fromJSON (A.String v) of
  A.Success t -> Just t
  A.Error _ -> Nothing

hasTextParam :: Text -> Maybe A.Value -> Bool
hasTextParam key = isJust . textParam key

defaultMaxTextLength :: Int
defaultMaxTextLength = 1000

defaultMaxFiles :: Int
defaultMaxFiles = 3
