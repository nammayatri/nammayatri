module SharedLogic.RideFeedback.Eligibility
  ( QuestionDecision (..),
    RideEvaluation (..),
    evaluateRide,
    cityQuestions,
    servedConfigVersions,
    tossKeyOf,
    statusAllowed,
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
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.RideFeedbackConfig (RideFeedbackConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RideFeedbackResponse as QRFR
import Tools.Error

-- | Outcome of one question's own limits for a ride. Reasons are stable codes (shown in the dashboard preview):
-- RIDE_STATUS_NOT_ALLOWED, ALREADY_SHOWN.
data QuestionDecision = Eligible UTCTime | Excluded Text
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
evaluateRide :: UTCTime -> DRide.Ride -> DB.Booking -> [DRFC.RideFeedbackConfig] -> Flow RideEvaluation
evaluateRide now ride booking configs = do
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = ride.merchantOperatingCityId.getId}) Nothing
      >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  city <- CQMOC.findById ride.merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound ride.merchantOperatingCityId.getId)
  driverPerson <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  driverStats <- QDriverStats.findById (cast ride.driverId)
  rideResponses <- QRFR.findAllByRideId ride.id
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let ctx = buildRideFeedbackContext ContextInput {timeDiffFromUtc = transporterConfig.timeDiffFromUtc, ..}
  selection <- selectQuestions ride.merchantOperatingCityId (tossKeyOf ride booking) localTime ctx
  let responseByConfig = HM.fromList [(r.configId, r) | r <- rideResponses]
      selected = HS.fromList selection.questionKeys
      candidates = filter (\c -> not (isFollowUpOnly c) && HS.member c.questionKey selected) configs
      decisions = [(c, decideQuestion now ride responseByConfig c) | c <- candidates]
      eligibleTop = [c | (c, Eligible _) <- decisions]
      byKey = HM.fromList [(c.questionKey, c) | c <- configs]
      followUps = collectFollowUps byKey (\c -> isOpen responseByConfig c && statusAllowed ride.status c) eligibleTop
  pure RideEvaluation {context = ctx, logicVersion = selection.logicVersion, selectedKeys = selection.questionKeys, decisions, followUps}

-- | The city's questions as Config Pilot serves them to this ride (`Just True`: enabled ones only).
-- Pinning the ride's transaction keeps it on one Config Pilot version, so a staged change shows
-- one consistent variant to the rider and to the dashboard preview.
cityQuestions :: DB.Booking -> Id DMOC.MerchantOperatingCity -> Maybe Bool -> Flow [DRFC.RideFeedbackConfig]
cityQuestions booking merchantOpCityId enabled = do
  L.setOptionLocal TxnIdKey booking.transactionId
  getConfig (RideFeedbackConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, enabled, questionKey = Nothing}) Nothing

-- | The Config Pilot versions behind 'cityQuestions' for this ride (call it after): the staged
-- version and the base one, just the base when nothing is staged, [] when none is set up.
servedConfigVersions :: Id DMOC.MerchantOperatingCity -> Flow [Int]
servedConfigVersions merchantOpCityId = do
  let domain = LYT.DRIVER_CONFIG RideFeedbackConfig
  selected <- selectActiveElementVersions domain (cast merchantOpCityId)
  if null selected
    then maybeToList . fmap (.version) <$> CADLR.findBaseRolloutByMerchantOpCityAndDomain (cast merchantOpCityId) domain
    else pure selected

-- | Stable A/B key for the logic rollout: the rider, or the ride when the rider is unknown.
tossKeyOf :: DRide.Ride -> DB.Booking -> Text
tossKeyOf ride booking = maybe ride.id.getId (.getId) booking.riderId

decideQuestion ::
  UTCTime ->
  DRide.Ride ->
  HM.HashMap (Id DRFC.RideFeedbackConfig) DRFR.RideFeedbackResponse ->
  DRFC.RideFeedbackConfig ->
  QuestionDecision
decideQuestion now ride responseByConfig config
  | not (statusAllowed ride.status config) = Excluded "RIDE_STATUS_NOT_ALLOWED"
  | not (isOpen responseByConfig config) = Excluded "ALREADY_SHOWN"
  | otherwise = Eligible showAt
  where
    trigger = config.displayTrigger
    showAt = case ride.status of
      DRide.INPROGRESS -> addSeconds (trigger >>= (.showAfterSecondsFromRideStart)) (fromMaybe ride.createdAt ride.tripStartTime)
      DRide.NEW -> addSeconds (trigger >>= (.showAfterSecondsFromAssign)) ride.createdAt
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

statusAllowed :: DRide.RideStatus -> DRFC.RideFeedbackConfig -> Bool
statusAllowed status config = status `elem` fromMaybe [DRide.INPROGRESS] config.allowedRideStatuses

isFollowUpOnly :: DRFC.RideFeedbackConfig -> Bool
isFollowUpOnly = fromMaybe False . (.isFollowUpOnly)

-- | Open = not shown on this ride yet: a question is shown at most once per ride.
isOpen :: HM.HashMap (Id DRFC.RideFeedbackConfig) DRFR.RideFeedbackResponse -> DRFC.RideFeedbackConfig -> Bool
isOpen responseByConfig config = not (HM.member config.id responseByConfig)

isFinal :: DRFR.RideFeedbackResponseStatus -> Bool
isFinal = (/= DRFR.SHOWN)

priorityKey :: DRFC.RideFeedbackConfig -> (Int, Text)
priorityKey c = (fromMaybe defaultPriority c.priority, c.questionKey)

defaultPriority :: Int
defaultPriority = 100
