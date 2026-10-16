module SharedLogic.AppFeedback.Validation
  ( validateConfig,
    validatePatchedCity,
    questionPlacements,
    needsRide,
  )
where

import Data.List (nub, sort)
import qualified Data.Text as T
import qualified Domain.Types.AppFeedbackConfig as DAFC
import qualified Domain.Types.AppFeedbackResponse as DAFR
import Kernel.Prelude
import Lib.Feedback.Question (QuestionRules (..), validateQuestion)
import Lib.Feedback.Rule (unsupportedOperators)
import qualified Lib.Feedback.Types as LFT
import SharedLogic.AppFeedback.Placement (placementSubject)

-- | Problems with a Config Pilot patch of a city's questions, given the questions before and after
-- it (same order): a patch may change any question's content, but not which questions exist, their
-- ids or city.
validatePatchedCity :: [DAFC.AppFeedbackConfig] -> [DAFC.AppFeedbackConfig] -> [Text]
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
validateConfig :: [DAFC.AppFeedbackConfig] -> DAFC.AppFeedbackConfig -> [Text]
validateConfig = validateQuestion appRules

-- | What app questions add to the shared checks: placements, subject condition, timing and cooldown.
appRules :: QuestionRules DAFC.AppFeedbackConfig
appRules =
  QuestionRules
    { ownErrors = \c ->
        let placements = questionPlacements c
            followUpOnly = fromMaybe False c.isFollowUpOnly
         in check (followUpOnly || not (null placements)) "placements must not be empty (only a follow-up-only question has none)"
              <> check (length (nub placements) == length placements) "placements must not repeat"
              <> subjectConditionErrors c placements
              <> triggerErrors c
              <> check (maybe True (>= 1) c.cooldownDays) "cooldownDays must be at least 1",
      -- A follow-up opens in the card of the answer that opened it, wherever that is.
      followUpErrors = \_ _ -> [],
      actionTypeError = \_ actionType ->
        if needsRide actionType then Just "needs a ride, so it cannot be used on an app feedback question" else Nothing
    }
  where
    check cond msg = [msg | not cond]
    subjectConditionErrors c placements = case c.subjectCondition of
      Nothing -> []
      Just cond ->
        let ops = unsupportedOperators cond
         in check (null ops) ("subjectCondition uses unsupported operators: " <> T.intercalate ", " ops)
              <> check
                (not (null placements) && all ((/= DAFR.RIDER) . placementSubject) placements)
                "subjectCondition needs placements about a ticket or journey (TICKET_BOOKED, JOURNEY_COMPLETED)"
    triggerErrors c = case c.displayTrigger of
      Nothing -> []
      Just t ->
        check (maybe True (>= 0) t.showAfterSeconds) "displayTrigger.showAfterSeconds must not be negative"
          <> check (maybe True (> 0) t.autoDismissAfterSeconds) "displayTrigger.autoDismissAfterSeconds must be positive"

questionPlacements :: DAFC.AppFeedbackConfig -> [DAFC.AppFeedbackPlacement]
questionPlacements = fromMaybe [] . (.placements)

-- | Actions that act on a ride or its driver, which app feedback has none of. Exhaustive, so a new
-- action type does not compile until it is decided here.
needsRide :: LFT.FeedbackActionType -> Bool
needsRide = \case
  LFT.REPORT_ISSUE_TO_BPP -> True
  LFT.SAFETY_ESCALATION -> True
  LFT.CREATE_TICKET -> False
  LFT.L0_SENSITIVE_WORD_CHECK -> False
  LFT.SLACK_ALERT -> False
  LFT.NOTIFY_RIDER -> False
