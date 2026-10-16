module Domain.Action.Dashboard.Management.RideFeedback
  ( getRideFeedbackInspectPreview,
    getRideFeedbackInspectResponses,
    getRideFeedbackMeta,
  )
where

import qualified API.Types.ProviderPlatform.Management.RideFeedback as Common
import qualified Data.Aeson as A
import Data.List (sortOn)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RideFeedbackResponse as DRFR
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Feedback.Question (reportableIssueTypes)
import Lib.Feedback.Rule (supportedOperatorNames)
import Lib.Feedback.View (uiLayouts)
import SharedLogic.RideFeedback.Eligibility
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideFeedbackResponse as QRFR
import Tools.Error

-------------------------------------------------------------------------------
-- Journeys (looked up by ride or booking id)

-- | What the rider of this journey would get right now, and why each question of the city is in or out.
-- Read-only: runs the same pass as the questions API (dynamic logic + per-question limits) without caching.
getRideFeedbackInspectPreview :: ShortId DM.Merchant -> Context.City -> Text -> Flow Common.RideFeedbackPreviewRes
getRideFeedbackInspectPreview merchantShortId opCity rideOrBookingId = do
  (_, moc) <- resolveCity merchantShortId opCity
  journey <- findJourney moc rideOrBookingId
  -- What the rider is served: Config Pilot, pinned to the journey's transaction (disabled ones too, to explain them).
  configs <- cityQuestions journey.booking moc.id Nothing
  now <- getCurrentTime
  evaluation <- evaluateJourney now journey (filter (.enabled) configs)
  let followUpIds = map (.id) evaluation.followUps
      evaluate c
        | not c.enabled = notEligible c "DISABLED"
        | isFollowUpOnly c =
          if c.id `elem` followUpIds
            then Common.QuestionEvaluation {configId = c.id, questionKey = c.questionKey, eligible = True, isFollowUp = True, reason = Nothing, showAt = Nothing}
            else notEligible c "FOLLOW_UP_NOT_REACHED"
        | isNothing journey.stage = notEligible c "NOTHING_TO_ASK_NOW"
        | otherwise = case snd <$> find ((== c.id) . (.id) . fst) evaluation.decisions of
          Just (Eligible t) -> Common.QuestionEvaluation {configId = c.id, questionKey = c.questionKey, eligible = True, isFollowUp = False, reason = Nothing, showAt = Just t}
          Just (Excluded r) -> notEligible c r
          Nothing
            | not (stageAllowed journey c) -> notEligible c "STAGE_NOT_ALLOWED"
            | otherwise -> notEligible c "NOT_SELECTED_BY_LOGIC"
      notEligible c r = Common.QuestionEvaluation {configId = c.id, questionKey = c.questionKey, eligible = False, isFollowUp = isFollowUpOnly c, reason = Just r, showAt = Nothing}
  pure
    Common.RideFeedbackPreviewRes
      { bookingId = journey.booking.id.getId,
        rideId = (.id.getId) <$> journey.ride,
        rideStatus = (.status) <$> journey.ride,
        stage = journey.stage,
        secondsSinceBooking = floor (diffUTCTime now journey.bookedAt),
        logicVersion = evaluation.logicVersion,
        selectedQuestionKeys = evaluation.selectedKeys,
        context = maybe A.Null A.toJSON evaluation.context,
        evaluations = map evaluate (sortOn priorityKey configs)
      }

-- | Every answer of the journey (all its bookings and rides), oldest first.
getRideFeedbackInspectResponses :: ShortId DM.Merchant -> Context.City -> Text -> Flow [Common.RideFeedbackResponseItem]
getRideFeedbackInspectResponses merchantShortId opCity rideOrBookingId = do
  (_, moc) <- resolveCity merchantShortId opCity
  journey <- findJourney moc rideOrBookingId
  responses <- QRFR.findAllByTransactionId journey.transactionId
  pure $ map toResponseItem (sortOn (.createdAt) responses)

-- | The journey of a ride id or, failing that, of a booking id, in this merchant city.
findJourney :: DMOC.MerchantOperatingCity -> Text -> Flow Journey
findJourney moc rideOrBookingId = do
  mbRide <- QRide.findById (Id rideOrBookingId)
  booking <- case mbRide of
    Just ride -> QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
    Nothing -> QB.findById (Id rideOrBookingId) >>= fromMaybeM (InvalidRequest $ "No ride or booking " <> rideOrBookingId)
  unless (booking.merchantOperatingCityId == moc.id) $ throwError (InvalidRequest "Ride or booking does not belong to this merchant city")
  resolveJourney booking

-------------------------------------------------------------------------------
-- Dashboard form metadata

getRideFeedbackMeta :: ShortId DM.Merchant -> Context.City -> Flow Common.RideFeedbackMetaRes
getRideFeedbackMeta merchantShortId opCity = do
  void $ resolveCity merchantShortId opCity
  pure
    Common.RideFeedbackMetaRes
      { issueReportTypes = reportableIssueTypes,
        uiLayouts,
        supportedOperators = supportedOperatorNames
      }

-------------------------------------------------------------------------------
-- Helpers

resolveCity :: ShortId DM.Merchant -> Context.City -> Flow (DM.Merchant, DMOC.MerchantOperatingCity)
resolveCity merchantShortId opCity = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  moc <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  pure (merchant, moc)

toResponseItem :: DRFR.RideFeedbackResponse -> Common.RideFeedbackResponseItem
toResponseItem r =
  Common.RideFeedbackResponseItem
    { id = r.id,
      configId = r.configId,
      questionKey = r.questionKey,
      questionType = r.questionType,
      bookingId = r.bookingId.getId,
      rideId = (.getId) <$> r.rideId,
      logicVersion = r.logicVersion,
      configPilotVersions = r.configPilotVersions,
      status = r.status,
      answer = r.answer,
      answerScore = r.answerScore,
      stage = r.stage,
      secondsSinceBooking = r.secondsSinceBooking,
      secondsIntoRide = r.secondsIntoRide,
      actionResults = r.actionResults,
      createdAt = r.createdAt,
      updatedAt = r.updatedAt
    }
