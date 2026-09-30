module SharedLogic.RideFeedback.Eligibility
  ( QuestionDecision (..),
    RideEvaluation (..),
    evaluateRide,
    fetchRiderConfig,
    statusAllowed,
    withinWindow,
    isOpen,
    isFinal,
    isFollowUpOnly,
    priorityKey,
    defaultPriority,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (nub)
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideFeedbackConfig as DRFC
import qualified Domain.Types.RideFeedbackResponse as DRFR
import qualified Domain.Types.RideStatus as DRS
import qualified Domain.Types.RiderConfig as DRC
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import SharedLogic.RideFeedback.Context
import SharedLogic.RideFeedback.Events (getRideFeedbackEvents)
import SharedLogic.RideFeedback.Selection (selectQuestions)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.RideFeedbackResponse as QRFR
import Tools.Error

-- | Outcome of one question's own limits for a ride. Reasons are stable codes (shown in the dashboard preview):
--   Excluded: OUTSIDE_WINDOW, RIDE_STATUS_NOT_ALLOWED, ALREADY_CLOSED, COOLDOWN, PAST_PROGRESS_WINDOW
--   Pending:  WAITING_FOR_EVENT, WAITING_FOR_PROGRESS
data QuestionDecision = Eligible UTCTime | Pending Text | Excluded Text
  deriving (Show)

data RideEvaluation = RideEvaluation
  { context :: RideFeedbackContext,
    logicVersion :: Maybe Int,
    -- | Keys returned by the RIDE-FEEDBACK logic, in its order.
    selectedKeys :: [Text],
    -- | Selected top-level questions and their decision.
    decisions :: [(DRFC.RideFeedbackConfig, QuestionDecision)],
    -- | Follow-ups reachable from the eligible questions.
    followUps :: [DRFC.RideFeedbackConfig]
  }

-- | The eligibility pass for one ride: build the context, let the RIDE-FEEDBACK dynamic logic pick question
-- keys, then apply each picked question's own limits from ride_feedback_config.
evaluateRide :: UTCTime -> DP.Person -> DRide.Ride -> DB.Booking -> [DRFC.RideFeedbackConfig] -> Flow RideEvaluation
evaluateRide now person ride booking configs = do
  riderConfig <- fetchRiderConfig booking
  city <- CQMOC.findById booking.merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  rideResponses <- QRFR.findAllByRideId ride.id
  let maxCooldownDays = maximum (0 : mapMaybe (.cooldownDays) configs)
  historyResponses <-
    if maxCooldownDays > 0
      then filter ((/= ride.id) . (.rideId)) <$> QRFR.findAllByPersonIdSince person.id (addUTCTime (fromIntegral $ negate maxCooldownDays * 86400) now)
      else pure []
  rideEvents <-
    if any (\c -> isJust (c.displayTrigger >>= (.triggerEvent))) configs
      then getRideFeedbackEvents ride.id
      else pure []
  let ctx = buildRideFeedbackContext ContextInput {timeDiffFromUtc = riderConfig.timeDiffFromUtc, ..}
      localTime = addUTCTime (fromIntegral riderConfig.timeDiffFromUtc.getSeconds) now
  selection <- selectQuestions booking.merchantOperatingCityId person.id localTime ctx
  let responseByConfig = HM.fromList [(r.configId, r) | r <- rideResponses]
      selected = HS.fromList selection.questionKeys
      candidates = filter (\c -> not (isFollowUpOnly c) && HS.member c.questionKey selected) configs
      decisions = [(c, decideQuestion now ride responseByConfig historyResponses rideEvents ctx c) | c <- candidates]
      eligibleTop = [c | (c, Eligible _) <- decisions]
      byKey = HM.fromList [(c.questionKey, c) | c <- configs]
      followUps = collectFollowUps byKey (\c -> isOpen responseByConfig c && statusAllowed ride.status c && withinWindow now c) eligibleTop
  pure RideEvaluation {context = ctx, logicVersion = selection.logicVersion, selectedKeys = selection.questionKeys, decisions, followUps}

decideQuestion ::
  UTCTime ->
  DRide.Ride ->
  HM.HashMap (Id DRFC.RideFeedbackConfig) DRFR.RideFeedbackResponse ->
  [DRFR.RideFeedbackResponse] ->
  [Text] ->
  RideFeedbackContext ->
  DRFC.RideFeedbackConfig ->
  QuestionDecision
decideQuestion now ride responseByConfig history rideEvents ctx config
  | not (withinWindow now config) = Excluded "OUTSIDE_WINDOW"
  | not (statusAllowed ride.status config) = Excluded "RIDE_STATUS_NOT_ALLOWED"
  | not (isOpen responseByConfig config) = Excluded "ALREADY_CLOSED"
  | inCooldown = Excluded "COOLDOWN"
  | maybe False (`notElem` rideEvents) (trigger >>= (.triggerEvent)) = Pending "WAITING_FOR_EVENT"
  | maybe False (\maxPct -> maybe False (> maxPct) progress) (trigger >>= (.maxDistanceCoveredPct)) = Excluded "PAST_PROGRESS_WINDOW"
  | maybe False (\minPct -> maybe True (< minPct) progress) (trigger >>= (.minDistanceCoveredPct)) = Pending "WAITING_FOR_PROGRESS"
  | otherwise = Eligible showAt
  where
    trigger = config.displayTrigger
    progress = ctx.derived.distanceCoveredPct
    inCooldown = case config.cooldownDays of
      Just days | days > 0 -> any (\r -> r.questionKey == config.questionKey && r.createdAt >= addUTCTime (fromIntegral $ negate days * 86400) now) history
      _ -> False
    showAt = case ride.status of
      DRS.INPROGRESS -> addSeconds (trigger >>= (.showAfterSecondsFromRideStart)) (fromMaybe ride.createdAt ride.rideStartTime)
      DRS.NEW -> addSeconds (trigger >>= (.showAfterSecondsFromAssign)) ride.createdAt
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

fetchRiderConfig :: DB.Booking -> Flow DRC.RiderConfig
fetchRiderConfig booking =
  getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
    >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)

statusAllowed :: DRS.RideStatus -> DRFC.RideFeedbackConfig -> Bool
statusAllowed status config = status `elem` fromMaybe [DRS.INPROGRESS] config.allowedRideStatuses

withinWindow :: UTCTime -> DRFC.RideFeedbackConfig -> Bool
withinWindow now config = maybe True (<= now) config.startsAt && maybe True (now <) config.endsAt

isFollowUpOnly :: DRFC.RideFeedbackConfig -> Bool
isFollowUpOnly = fromMaybe False . (.isFollowUpOnly)

-- | Open = never shown, or shown fewer times than allowed and not closed by an answer/skip/dismiss.
isOpen :: HM.HashMap (Id DRFC.RideFeedbackConfig) DRFR.RideFeedbackResponse -> DRFC.RideFeedbackConfig -> Bool
isOpen responseByConfig config = case HM.lookup config.id responseByConfig of
  Nothing -> True
  Just r -> not (isFinal r.status) && r.shownCount < fromMaybe 1 config.maxShowsPerRide

isFinal :: DRFR.RideFeedbackResponseStatus -> Bool
isFinal = (/= DRFR.SHOWN)

priorityKey :: DRFC.RideFeedbackConfig -> (Int, Text)
priorityKey c = (fromMaybe defaultPriority c.priority, c.questionKey)

defaultPriority :: Int
defaultPriority = 100
