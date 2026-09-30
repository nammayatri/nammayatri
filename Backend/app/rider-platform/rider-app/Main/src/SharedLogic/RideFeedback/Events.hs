module SharedLogic.RideFeedback.Events
  ( recordRideFeedbackEvent,
    getRideFeedbackEvents,
    clearEligibleQuestionsCache,
    eligibleQuestionsCacheKey,
  )
where

import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common

-- | Marks that something happened during a ride (e.g. "RIDE_STOPPAGE"). Questions whose
-- displayTrigger.triggerEvent names this event become eligible on the rider's next poll.
recordRideFeedbackEvent :: CacheFlow m r => Id DRide.Ride -> Text -> m ()
recordRideFeedbackEvent rideId event = do
  Hedis.sAddExp (eventsKey rideId) [event] eventsTtl
  clearEligibleQuestionsCache rideId

getRideFeedbackEvents :: CacheFlow m r => Id DRide.Ride -> m [Text]
getRideFeedbackEvents = Hedis.sMembers . eventsKey

clearEligibleQuestionsCache :: CacheFlow m r => Id DRide.Ride -> m ()
clearEligibleQuestionsCache = Hedis.del . eligibleQuestionsCacheKey

eligibleQuestionsCacheKey :: Id DRide.Ride -> Text
eligibleQuestionsCacheKey rideId = "RideFeedback:EligibleQuestions:RideId-" <> rideId.getId

eventsKey :: Id DRide.Ride -> Text
eventsKey rideId = "RideFeedback:Events:RideId-" <> rideId.getId

-- | Longer than any ride; the set only needs to live while the ride is active.
eventsTtl :: Hedis.ExpirationTime
eventsTtl = 86400
