module SharedLogic.RideFeedback.Eligibility
  ( Journey (..),
    resolveJourney,
    subjectOf,
    currentResponses,
    stageAllowed,
    QuestionDecision (..),
    JourneyEvaluation (..),
    evaluateJourney,
    cityQuestions,
    servedConfigVersions,
    isOpen,
    isFinal,
    isFollowUpOnly,
    priorityKey,
    defaultPriority,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideFeedbackConfig as DRFC
import qualified Domain.Types.RideFeedbackResponse as DRFR
import Environment
import qualified EulerHS.Language as L
import Kernel.Beam.Types (TxnIdKey (..))
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Config.GetterInternal (selectActiveElementVersions)
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as CADLR
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (RideFeedbackConfig))
import SharedLogic.RideFeedback.Context
import SharedLogic.RideFeedback.Selection (selectQuestions)
import SharedLogic.RideFeedback.Validation (isBookingQuestion, questionStages)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.RideFeedbackConfig (RideFeedbackConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideFeedbackResponse as QRFR
import Tools.Error

-- | A rider's journey from booking to drop-off, as feedback sees it. A reallocation can replace the
-- driver-side booking (same transactionId) or reuse it with a new ride, so everything is resolved
-- from the transaction: its latest booking and that booking's live ride.
data Journey = Journey
  { transactionId :: Text,
    -- | The journey's current booking.
    booking :: DB.Booking,
    -- | When the journey was first booked (a reallocated booking is newer).
    bookedAt :: UTCTime,
    -- | The current booking's latest ride that was not cancelled, once a driver is assigned.
    ride :: Maybe DRide.Ride,
    -- | Where the journey is now; Nothing when nothing can be asked (scheduled ride not started yet,
    -- ride completed, booking cancelled).
    stage :: Maybe DRFC.RideFeedbackStage
  }

resolveJourney :: DB.Booking -> Flow Journey
resolveJourney anyBooking = do
  bookings <- QB.findAllByTransactionId anyBooking.transactionId
  let current = fromMaybe anyBooking (lastMaybe bookings)
      bookedAt = maybe anyBooking.createdAt (.createdAt) (listToMaybe bookings)
  rides <- QRide.findRidesByBookingId (map (.id) (if null bookings then [anyBooking] else bookings))
  let live = listToMaybe $ sortOn (Down . (.createdAt)) [r | r <- rides, r.bookingId == current.id, r.status /= DRide.CANCELLED]
      driverCancelled = any ((== DRide.CANCELLED) . (.status)) rides
      stage = case live of
        Just r -> case r.status of
          DRide.NEW -> Just DRFC.RIDE_ASSIGNED
          DRide.INPROGRESS -> Just DRFC.RIDE_IN_PROGRESS
          _ -> Nothing
        Nothing
          | current.status == DB.NEW -> Just (if driverCancelled then DRFC.AWAITING_REASSIGNMENT else DRFC.BOOKING_CONFIRMED)
          | otherwise -> Nothing
  pure Journey {transactionId = current.transactionId, booking = current, bookedAt, ride = live, stage}
  where
    lastMaybe xs = if null xs then Nothing else Just (last xs)

-- | What a question's answer is about on this journey: the journey for booking-stage questions, the
-- ride for ride-stage ones (Nothing until a driver is assigned). A question is asked once per subject.
subjectOf :: Journey -> DRFC.RideFeedbackConfig -> Maybe Text
subjectOf journey config
  | isBookingQuestion config = Just journey.transactionId
  | otherwise = (.id.getId) <$> journey.ride

-- | The journey's responses to questions on their current subject, by question: answers about an
-- earlier ride (before a reallocation) are left out.
currentResponses :: Journey -> [DRFR.RideFeedbackResponse] -> HM.HashMap (Id DRFC.RideFeedbackConfig) DRFR.RideFeedbackResponse
currentResponses journey responses =
  HM.fromList [(r.configId, r) | r <- responses, r.subjectId `elem` currentSubjects]
  where
    currentSubjects = journey.transactionId : maybeToList ((.id.getId) <$> journey.ride)

stageAllowed :: Journey -> DRFC.RideFeedbackConfig -> Bool
stageAllowed journey config = maybe False (`elem` questionStages config) journey.stage

-- | Outcome of one question's own limits for a journey. Reasons are stable codes (shown in the dashboard preview):
-- STAGE_NOT_ALLOWED, ALREADY_SHOWN.
data QuestionDecision = Eligible UTCTime | Excluded Text
  deriving (Show)

data JourneyEvaluation = JourneyEvaluation
  { context :: Maybe RideFeedbackContext,
    logicVersion :: Maybe Int,
    -- | Keys returned by the RIDE-FEEDBACK logic, in its order.
    selectedKeys :: [Text],
    -- | Selected top-level questions and their decision.
    decisions :: [(DRFC.RideFeedbackConfig, QuestionDecision)],
    -- | Follow-ups reachable from the eligible questions.
    followUps :: [DRFC.RideFeedbackConfig]
  }

-- | The eligibility pass for one journey: build the context, let the RIDE-FEEDBACK dynamic logic pick question
-- keys, then apply each picked question's own limits from ride_feedback_config. Nothing is selected
-- while the journey has no stage (the context then is Nothing).
evaluateJourney :: UTCTime -> Journey -> [DRFC.RideFeedbackConfig] -> Flow JourneyEvaluation
evaluateJourney now journey configs = case journey.stage of
  Nothing -> pure JourneyEvaluation {context = Nothing, logicVersion = Nothing, selectedKeys = [], decisions = [], followUps = []}
  Just stage -> do
    let booking = journey.booking
    transporterConfig <-
      getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
        >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
    city <- CQMOC.findById booking.merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
    rideWithDriver <- forM journey.ride $ \ride -> do
      driverPerson <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      driverStats <- QDriverStats.findById (cast ride.driverId)
      pure (ride, driverPerson, driverStats)
    responses <- QRFR.findAllByTransactionId journey.transactionId
    localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
    let ctx = buildRideFeedbackContext ContextInput {timeDiffFromUtc = transporterConfig.timeDiffFromUtc, bookedAt = journey.bookedAt, ..}
        tossKey = maybe booking.id.getId (.getId) booking.riderId
    selection <- selectQuestions booking.merchantOperatingCityId tossKey localTime ctx
    let responseByConfig = currentResponses journey responses
        selected = HS.fromList selection.questionKeys
        candidates = filter (\c -> not (isFollowUpOnly c) && HS.member c.questionKey selected) configs
        decisions = [(c, decideQuestion now journey responseByConfig c) | c <- candidates]
        eligibleTop = [c | (c, Eligible _) <- decisions]
        byKey = HM.fromList [(c.questionKey, c) | c <- configs]
        followUps = collectFollowUps byKey (\c -> isOpen responseByConfig c && stageAllowed journey c) eligibleTop
    pure JourneyEvaluation {context = Just ctx, logicVersion = selection.logicVersion, selectedKeys = selection.questionKeys, decisions, followUps}

-- | The city's questions as Config Pilot serves them to this journey (`Just True`: enabled ones only).
-- Pinning the booking's transaction keeps it on one Config Pilot version, so the rider and the
-- dashboard preview see one consistent variant.
cityQuestions :: DB.Booking -> Id DMOC.MerchantOperatingCity -> Maybe Bool -> Flow [DRFC.RideFeedbackConfig]
cityQuestions booking merchantOpCityId enabled = do
  L.setOptionLocal TxnIdKey booking.transactionId
  getConfig (RideFeedbackConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, enabled, questionKey = Nothing}) Nothing

-- | The Config Pilot versions behind 'cityQuestions' for this journey (call it after): the staged
-- version and the base one, just the base when nothing is staged, [] when none is set up.
servedConfigVersions :: Id DMOC.MerchantOperatingCity -> Flow [Int]
servedConfigVersions merchantOpCityId = do
  let domain = LYT.DRIVER_CONFIG RideFeedbackConfig
  selected <- selectActiveElementVersions domain (cast merchantOpCityId)
  if null selected
    then maybeToList . fmap (.version) <$> CADLR.findBaseRolloutByMerchantOpCityAndDomain (cast merchantOpCityId) domain
    else pure selected

decideQuestion ::
  UTCTime ->
  Journey ->
  HM.HashMap (Id DRFC.RideFeedbackConfig) DRFR.RideFeedbackResponse ->
  DRFC.RideFeedbackConfig ->
  QuestionDecision
decideQuestion now journey responseByConfig config
  | not (stageAllowed journey config) = Excluded "STAGE_NOT_ALLOWED"
  | not (isOpen responseByConfig config) = Excluded "ALREADY_SHOWN"
  | otherwise = Eligible showAt
  where
    trigger = config.displayTrigger
    -- Each delay counts from the start of its stage: booking, driver assignment, ride start.
    showAt = case (journey.stage, journey.ride) of
      (Just DRFC.RIDE_IN_PROGRESS, Just ride) -> addSeconds (trigger >>= (.showAfterSecondsFromRideStart)) (fromMaybe ride.createdAt ride.tripStartTime)
      (Just DRFC.RIDE_ASSIGNED, Just ride) -> addSeconds (trigger >>= (.showAfterSecondsFromAssign)) ride.createdAt
      (Just DRFC.BOOKING_CONFIRMED, _) -> addSeconds (trigger >>= (.showAfterSecondsFromBooking)) journey.bookedAt
      (Just DRFC.AWAITING_REASSIGNMENT, _) -> addSeconds (trigger >>= (.showAfterSecondsFromBooking)) journey.bookedAt
      _ -> now
    addSeconds mbSecs = addUTCTime (fromIntegral $ fromMaybe 0 mbSecs)

-- | Follow-ups reachable through option.nextQuestionKey, breadth-first, at most 3 levels deep.
-- They are not in the logic's output: a follow-up is shown only through the parent the logic selected.
collectFollowUps ::
  HM.HashMap Text DRFC.RideFeedbackConfig ->
  (DRFC.RideFeedbackConfig -> Bool) ->
  [DRFC.RideFeedbackConfig] ->
  [DRFC.RideFeedbackConfig]
collectFollowUps byKey usable roots = go (3 :: Int) (HS.fromList $ map (.questionKey) roots) roots
  where
    go 0 _ _ = []
    go depth seen parents =
      let nextKeys = nub [k | p <- parents, o <- fromMaybe [] p.options, Just k <- [o.nextQuestionKey], not (HS.member k seen)]
          level = filter usable $ mapMaybe (`HM.lookup` byKey) nextKeys
       in if null level then [] else level <> go (depth - 1) (HS.union seen (HS.fromList nextKeys)) level

isFollowUpOnly :: DRFC.RideFeedbackConfig -> Bool
isFollowUpOnly = fromMaybe False . (.isFollowUpOnly)

-- | Open = not shown on its subject yet: a question is shown at most once per ride (ride stages) or
-- journey (booking stages).
isOpen :: HM.HashMap (Id DRFC.RideFeedbackConfig) DRFR.RideFeedbackResponse -> DRFC.RideFeedbackConfig -> Bool
isOpen responseByConfig config = not (HM.member config.id responseByConfig)

isFinal :: DRFR.RideFeedbackResponseStatus -> Bool
isFinal = (/= DRFR.SHOWN)

priorityKey :: DRFC.RideFeedbackConfig -> (Int, Text)
priorityKey c = (fromMaybe defaultPriority c.priority, c.questionKey)

defaultPriority :: Int
defaultPriority = 100
