module SharedLogic.RideFeedback.Validation
  ( validateConfig,
    validatePatchedCity,
    reportableIssueTypes,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAsciiUpper, isDigit)
import Data.List (nub, sort, (\\))
import qualified Data.Text as T
import qualified Domain.Types.Ride as DRS
import qualified Domain.Types.RideFeedbackConfig as DRFC
import IssueManagement.Common (IssueReportType (..))
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import SharedLogic.RideFeedback.Rule (unsupportedOperators)

-- | Problems with a Config Pilot patch of a city's questions, given the questions before and after
-- it (same order): a patch may change any question's content, but not which questions exist, their
-- ids or city.
validatePatchedCity :: [DRFC.RideFeedbackConfig] -> [DRFC.RideFeedbackConfig] -> [Text]
validatePatchedCity before after =
  check (sort (map (.id.getId) before) == sort (map (.id.getId) after)) "a patch cannot add, remove or re-id questions"
    <> check (all (\(b, a) -> b.merchantOperatingCityId == a.merchantOperatingCityId && b.merchantId == a.merchantId) (zip before after)) "a patch cannot move a question to another city"
    <> check (length (nub keys) == length keys) "questionKeys must stay unique in the city"
    <> concat [map ((c.questionKey <> ": ") <>) (validateConfig after c) | c <- after]
  where
    keys = map (.questionKey) after
    check cond msg = [msg | not cond]

-- | Human-readable problems with a question config; empty means valid.
-- 'cityConfigs' are the other questions of the same city (to resolve option.nextQuestionKey).
validateConfig :: [DRFC.RideFeedbackConfig] -> DRFC.RideFeedbackConfig -> [Text]
validateConfig cityConfigs c =
  concat
    [ check (validKey c.questionKey) "questionKey must be 2-64 characters of A-Z, 0-9 and _, starting with a letter",
      translations "title" (Just c.title),
      translations "description" c.description,
      translations "acknowledgement" c.acknowledgement,
      optionErrors,
      inputConfigErrors,
      check (maybe True (\s -> not (null s) && all (`elem` [DRS.NEW, DRS.INPROGRESS]) s) c.allowedRideStatuses) "allowedRideStatuses must be a non-empty subset of NEW and INPROGRESS",
      triggerErrors,
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
    otherKeys = [x.questionKey | x <- cityConfigs, x.id /= c.id]
    optionErrors
      | usesOptions c.questionType =
        check (not (null options)) (show c.questionType <> " needs at least one option")
          <> check (length (nub optionKeys) == length optionKeys) "option keys must be unique"
          <> check (all validKey optionKeys) "option keys must be 2-64 characters of A-Z, 0-9 and _, starting with a letter"
          <> concatMap (\o -> translations ("option " <> o.key <> " label") (Just o.label)) options
          <> concat
            [ check (next /= c.questionKey) ("option " <> o.key <> " cannot open its own question")
                <> check (next `elem` otherKeys) ("option " <> o.key <> " opens " <> next <> ", which does not exist in this city")
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

    triggerErrors = case c.displayTrigger of
      Nothing -> []
      Just t ->
        check (all (>= 0) (catMaybes [t.showAfterSecondsFromRideStart, t.showAfterSecondsFromAssign])) "displayTrigger show-after seconds must not be negative"
          <> check (maybe True (> 0) t.autoDismissAfterSeconds) "displayTrigger.autoDismissAfterSeconds must be positive"

    rules = fromMaybe [] c.actionRules
    ruleIds = map (.ruleId) rules
    actionRuleErrors =
      check (not (any (T.null . T.strip) ruleIds)) "every action rule needs a ruleId"
        <> check (null (ruleIds \\ nub ruleIds)) "action rule ids must be unique"
        <> concatMap ruleErrors rules
    ruleErrors rule =
      [ "action rule " <> rule.ruleId <> " uses unsupported operators: " <> T.intercalate ", " ops
        | Just cond <- [rule.condition],
          let ops = unsupportedOperators cond,
          not (null ops)
      ]
        <> check (not (null rule.actions)) ("action rule " <> rule.ruleId <> " has no actions")
        <> concatMap (actionErrors rule.ruleId) rule.actions
    actionErrors ruleId action =
      let needs key = check (hasTextParam key action.params) ("action " <> show action.actionType <> " in rule " <> ruleId <> " needs param " <> key)
       in case action.actionType of
            DRFC.REPORT_ISSUE_TO_BPP ->
              check
                (maybe False (`elem` reportableIssueTypes) (textParam "issueType" action.params >>= parseIssueType))
                ("action REPORT_ISSUE_TO_BPP in rule " <> ruleId <> " needs param issueType, one of " <> T.intercalate ", " (map show reportableIssueTypes))
            DRFC.CREATE_TICKET -> needs "category"
            DRFC.NOTIFY_RIDER -> needs "notificationKey"
            _ -> []

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

-- | Question types answered by picking option keys (they need 'options'). Exhaustive, so a new
-- question type does not compile until it is placed here.
usesOptions :: DRFC.RideFeedbackQuestionType -> Bool
usesOptions = \case
  DRFC.SINGLE_SELECT -> True
  DRFC.MULTI_SELECT -> True
  DRFC.YES_NO -> True
  DRFC.THUMBS -> True
  DRFC.EMOJI_SCALE -> True
  DRFC.STAR_RATING -> False
  DRFC.SCALE -> False
  DRFC.TEXT_INPUT -> False
  DRFC.NUMBER_INPUT -> False
  DRFC.IMAGE_UPLOAD -> False
  DRFC.AUDIO -> False

-- | Issue types the BPP's /internal/{rideId}/reportIssue acts on (the dashboard offers these).
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
