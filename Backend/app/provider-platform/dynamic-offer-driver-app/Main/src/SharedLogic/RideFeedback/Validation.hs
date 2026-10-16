module SharedLogic.RideFeedback.Validation
  ( validateConfig,
    validatePatchedCity,
    questionStages,
    isRideStage,
    isBookingQuestion,
    needsRide,
  )
where

import Data.List (nub, sort)
import qualified Domain.Types.RideFeedbackConfig as DRFC
import Kernel.Prelude
import Lib.Feedback.Question (QuestionRules (..), validateQuestion)
import qualified Lib.Feedback.Types as LFT

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
validateConfig = validateQuestion rideRules

-- | What ride questions add to the shared checks: stages, timing and the booking/ride split.
rideRules :: QuestionRules DRFC.RideFeedbackConfig
rideRules =
  QuestionRules
    { ownErrors = \c ->
        let stages = questionStages c
         in check (maybe True (not . null) c.stages) "stages must not be empty"
              <> check (all isRideStage stages || not (any isRideStage stages)) "stages cannot mix booking stages (BOOKING_CONFIRMED, AWAITING_REASSIGNMENT) with ride stages (RIDE_ASSIGNED, RIDE_IN_PROGRESS)"
              <> triggerErrors c,
      followUpErrors = \parent target ->
        ["which is asked at a different level (booking vs ride)" | isBookingQuestion parent /= isBookingQuestion target],
      actionTypeError = \c actionType ->
        if isBookingQuestion c && needsRide actionType
          then Just "needs a ride, so it cannot be used on a booking-stage question"
          else Nothing
    }
  where
    check cond msg = [msg | not cond]
    triggerErrors c = case c.displayTrigger of
      Nothing -> []
      Just t ->
        check (all (>= 0) (catMaybes [t.showAfterSecondsFromBooking, t.showAfterSecondsFromRideStart, t.showAfterSecondsFromAssign])) "displayTrigger show-after seconds must not be negative"
          <> check (maybe True (> 0) t.autoDismissAfterSeconds) "displayTrigger.autoDismissAfterSeconds must be positive"

-- | Where a question can be asked; ride in progress when not set.
questionStages :: DRFC.RideFeedbackConfig -> [DRFC.RideFeedbackStage]
questionStages = fromMaybe [DRFC.RIDE_IN_PROGRESS] . (.stages)

-- | Exhaustive, so a new stage does not compile until it is placed here.
isRideStage :: DRFC.RideFeedbackStage -> Bool
isRideStage = \case
  DRFC.BOOKING_CONFIRMED -> False
  DRFC.AWAITING_REASSIGNMENT -> False
  DRFC.RIDE_ASSIGNED -> True
  DRFC.RIDE_IN_PROGRESS -> True

-- | Asked before any driver: the answer is about the booking (once per journey), not a ride or driver.
isBookingQuestion :: DRFC.RideFeedbackConfig -> Bool
isBookingQuestion = not . any isRideStage . questionStages

-- | Actions that act on the ride or its driver. Exhaustive, so a new action type does not compile until it is decided here.
needsRide :: LFT.FeedbackActionType -> Bool
needsRide = \case
  LFT.REPORT_ISSUE_TO_BPP -> True
  LFT.SAFETY_ESCALATION -> True
  LFT.CREATE_TICKET -> False
  LFT.L0_SENSITIVE_WORD_CHECK -> False
  LFT.SLACK_ALERT -> False
  LFT.NOTIFY_RIDER -> False
