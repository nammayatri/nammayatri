module Domain.Action.Dashboard.Management.RideFeedback
  ( getRideFeedbackRidePreview,
    getRideFeedbackRideResponses,
    getRideFeedbackMeta,
  )
where

import qualified API.Types.ProviderPlatform.Management.RideFeedback as Common
import qualified Dashboard.Common as DCommon
import qualified Data.Aeson as A
import Data.List (sortOn)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideFeedbackResponse as DRFR
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.RideFeedback.Eligibility
import SharedLogic.RideFeedback.Rule (supportedOperatorNames)
import SharedLogic.RideFeedback.Validation (reportableIssueTypes)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideFeedbackResponse as QRFR
import Tools.Error

-------------------------------------------------------------------------------
-- Rides

-- | What the rider of this ride would get right now, and why each question of the city is in or out.
-- Read-only: runs the same pass as the questions API (dynamic logic + per-question limits) without caching.
getRideFeedbackRidePreview :: ShortId DM.Merchant -> Context.City -> Id DCommon.Ride -> Flow Common.RideFeedbackPreviewRes
getRideFeedbackRidePreview merchantShortId opCity dashboardRideId = do
  let rideId = cast dashboardRideId :: Id DRide.Ride
  (_, moc) <- resolveCity merchantShortId opCity
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  unless (ride.merchantOperatingCityId == moc.id) $ throwError (InvalidRequest "Ride does not belong to this merchant city")
  -- What the rider is served: Config Pilot, pinned to this ride's transaction (disabled ones too, to explain them).
  configs <- cityQuestions booking moc.id Nothing
  now <- getCurrentTime
  evaluation <- evaluateRide now ride booking (filter (.enabled) configs)
  let followUpIds = map (.id) evaluation.followUps
      evaluate c
        | not c.enabled = notEligible c "DISABLED"
        | isFollowUpOnly c =
          if c.id `elem` followUpIds
            then Common.QuestionEvaluation {configId = c.id, questionKey = c.questionKey, eligible = True, isFollowUp = True, reason = Nothing, showAt = Nothing}
            else notEligible c "FOLLOW_UP_NOT_REACHED"
        | otherwise = case snd <$> find ((== c.id) . (.id) . fst) evaluation.decisions of
          Just (Eligible t) -> Common.QuestionEvaluation {configId = c.id, questionKey = c.questionKey, eligible = True, isFollowUp = False, reason = Nothing, showAt = Just t}
          Just (Excluded r) -> notEligible c r
          Nothing -> notEligible c "NOT_SELECTED_BY_LOGIC"
      notEligible c r = Common.QuestionEvaluation {configId = c.id, questionKey = c.questionKey, eligible = False, isFollowUp = isFollowUpOnly c, reason = Just r, showAt = Nothing}
  pure
    Common.RideFeedbackPreviewRes
      { rideId = dashboardRideId,
        rideStatus = ride.status,
        logicVersion = evaluation.logicVersion,
        selectedQuestionKeys = evaluation.selectedKeys,
        context = A.toJSON evaluation.context,
        evaluations = map evaluate (sortOn priorityKey configs)
      }

getRideFeedbackRideResponses :: ShortId DM.Merchant -> Context.City -> Id DCommon.Ride -> Flow [Common.RideFeedbackResponseItem]
getRideFeedbackRideResponses merchantShortId opCity dashboardRideId = do
  (_, moc) <- resolveCity merchantShortId opCity
  responses <- filter ((== moc.id) . (.merchantOperatingCityId)) <$> QRFR.findAllByRideId (cast dashboardRideId)
  pure $ map toResponseItem (sortOn (.createdAt) responses)

-------------------------------------------------------------------------------
-- Dashboard form metadata

getRideFeedbackMeta :: ShortId DM.Merchant -> Context.City -> Flow Common.RideFeedbackMetaRes
getRideFeedbackMeta merchantShortId opCity = do
  void $ resolveCity merchantShortId opCity
  pure
    Common.RideFeedbackMetaRes
      { allowedRideStatuses = [DRide.NEW, DRide.INPROGRESS],
        issueReportTypes = reportableIssueTypes,
        uiLayouts = ["inline_card", "bottom_sheet", "chips", "poll"],
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
      logicVersion = r.logicVersion,
      configPilotVersions = r.configPilotVersions,
      driverId = r.driverId.getId,
      status = r.status,
      answer = r.answer,
      parentResponseId = r.parentResponseId,
      rideStatusAtResponse = r.rideStatusAtResponse,
      secondsIntoRide = r.secondsIntoRide,
      actionResults = r.actionResults,
      createdAt = r.createdAt,
      updatedAt = r.updatedAt
    }
