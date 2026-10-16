-- | Which app feedback questions a rider gets, and whether a response to one can be stored.
--
-- Two passes, so the app can fetch questions once per session and still show each card only where it
-- should be:
--   * the manifest ('evaluateRider'): the APP-FEEDBACK logic on the rider (no subject), then each
--     question's own limits (placements, cooldown);
--   * a response at a placement ('decideAtPlacement'): the question must be asked there, not already
--     finished on its subject, match its subjectCondition and, when it opens a new card, fit the cap.
module SharedLogic.AppFeedback.Eligibility
  ( RiderState (..),
    loadRiderState,
    cityQuestions,
    servedConfigVersions,
    riderContext,
    Evaluation (..),
    Decision (..),
    evaluateRider,
    riderPlacements,
    subjectResponses,
    decideAtPlacement,
    isFinal,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Time as Time
import qualified Domain.Types.AppFeedbackConfig as DAFC
import qualified Domain.Types.AppFeedbackResponse as DAFR
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RiderConfig as DRC
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Config.GetterInternal (selectActiveElementVersions)
import Lib.ConfigPilot.Interface.Types (getConfig)
import Lib.Feedback.Question (collectFollowUps, isFollowUpOnly)
import Lib.Feedback.Rule (evaluateRule)
import Lib.Feedback.Selection (selectQuestions)
import qualified Lib.Feedback.Types as LFT
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogicRollout as CADLR
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.AppFeedback.Cap as Cap
import SharedLogic.AppFeedback.Context
import SharedLogic.AppFeedback.Placement (placementSubject)
import SharedLogic.AppFeedback.Validation (questionPlacements)
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.AppFeedbackConfig (AppFeedbackConfigDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.AppFeedbackResponse as QAFR
import qualified Storage.Queries.Person as QPerson
import Tools.Error

-- | Everything about the rider the passes need, read once per request.
data RiderState = RiderState
  { person :: DP.Person,
    city :: DMOC.MerchantOperatingCity,
    riderConfig :: DRC.RiderConfig,
    now :: UTCTime,
    -- | The rider's latest app feedback responses, newest first.
    responses :: [DAFR.AppFeedbackResponse],
    -- | When feedback cards (app and ride) were shown in the last 7 days.
    shownTimes :: [UTCTime],
    nextAllowedAt :: Maybe UTCTime
  }

loadRiderState :: Id DP.Person -> Flow RiderState
loadRiderState personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  city <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  riderConfig <-
    getConfig (RiderConfigDimensions {merchantOperatingCityId = city.id.getId}) Nothing
      >>= fromMaybeM (RiderConfigDoesNotExist city.id.getId)
  now <- getCurrentTime
  responses <- QAFR.findLatestByPersonId (Just maxResponsesRead) Nothing personId
  shownTimes <- Cap.recentShownTimes personId now
  pure RiderState {nextAllowedAt = Cap.nextAllowedAt (Cap.capSettings riderConfig) now shownTimes, ..}

-- | The city's questions as Config Pilot serves them (`Just True`: enabled ones only).
cityQuestions :: Id DMOC.MerchantOperatingCity -> Maybe Bool -> Flow [DAFC.AppFeedbackConfig]
cityQuestions merchantOpCityId enabled =
  getConfig (AppFeedbackConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, enabled, questionKey = Nothing}) Nothing

-- | The Config Pilot versions behind 'cityQuestions' (stored with each response): the active version and
-- the base one, just the base when nothing else is active, [] when none is set up.
servedConfigVersions :: Id DMOC.MerchantOperatingCity -> Flow [Int]
servedConfigVersions merchantOpCityId = do
  let domain = LYT.RIDER_CONFIG LYT.AppFeedbackConfig
  selected <- selectActiveElementVersions domain (cast merchantOpCityId)
  if null selected
    then maybeToList . fmap (.version) <$> CADLR.findBaseRolloutByMerchantOpCityAndDomain (cast merchantOpCityId) domain
    else pure selected

-- | The rider's targeting context at a placement (Nothing: the manifest) with its subject.
riderContext :: RiderState -> Maybe DAFC.AppFeedbackPlacement -> Maybe SubjectCtx -> AppFeedbackContext
riderContext st placement subject =
  buildContext st.now st.riderConfig.timeDiffFromUtc st.person st.city cardsShownLast7Days daysSinceAnswered placement subject
  where
    cardsShownLast7Days = length st.shownTimes
    daysSinceAnswered =
      Map.fromListWith
        min
        [ (r.questionKey, floor (Time.diffUTCTime st.now r.updatedAt / 86400))
          | r <- st.responses,
            r.status == LFT.ANSWERED
        ]

-- | Outcome of one question's own limits. Reasons are stable codes (shown in the dashboard preview).
data Decision = Eligible | Excluded Text
  deriving (Show, Eq)

data Evaluation = Evaluation
  { context :: AppFeedbackContext,
    logicVersion :: Maybe Int,
    -- | Keys returned by the APP-FEEDBACK logic, in its order.
    selectedKeys :: [Text],
    -- | Selected top-level questions and their decision.
    decisions :: [(DAFC.AppFeedbackConfig, Decision)],
    -- | Follow-ups reachable from the selected questions.
    followUps :: [DAFC.AppFeedbackConfig]
  }

-- | The targeting pass: build the context, let the APP-FEEDBACK logic pick question keys, then apply
-- each picked question's own limits. With a placement, a question must be asked there; without one
-- (the manifest), at least one of its placements must still be open to the rider.
evaluateRider :: RiderState -> [DAFC.AppFeedbackConfig] -> Maybe DAFC.AppFeedbackPlacement -> Maybe SubjectCtx -> Flow Evaluation
evaluateRider st configs placement subject = do
  let ctx = riderContext st placement subject
  localTime <- getLocalCurrentTime st.riderConfig.timeDiffFromUtc
  selection <- selectQuestions LYDL.Rider LYT.APP_FEEDBACK (cast st.city.id) st.person.id.getId localTime ctx
  let selected = HS.fromList selection.questionKeys
      candidates = filter (\c -> not (isFollowUpOnly c) && HS.member c.questionKey selected) configs
      decide c = case placement of
        Just p | p `notElem` questionPlacements c -> Excluded "PLACEMENT_NOT_ALLOWED"
        Just _ -> Eligible
        Nothing -> if null (riderPlacements st c) then Excluded "ALREADY_ASKED" else Eligible
      decisions = [(c, decide c) | c <- candidates]
      byKey = HM.fromList [(c.questionKey, c) | c <- configs]
  pure
    Evaluation
      { context = ctx,
        logicVersion = selection.logicVersion,
        selectedKeys = selection.questionKeys,
        decisions,
        -- Of every selected question, also those already shown: a card on screen keeps its follow-ups when
        -- the app fetches again. A follow-up is stored only once its parent's answer opened it.
        followUps = collectFollowUps byKey (const True) candidates
      }

-- | The question's placements still open to the rider: a placement about a ticket or journey is open
-- (each subject is checked when the card is shown); a placement about the rider is open until the
-- question was asked, or again once its cooldown has passed.
riderPlacements :: RiderState -> DAFC.AppFeedbackConfig -> [DAFC.AppFeedbackPlacement]
riderPlacements st c = filter open (questionPlacements c)
  where
    open p = placementSubject p /= DAFR.RIDER || null (subjectResponses st c st.person.id.getId)

-- | The responses to a question on a subject that still count: all of them for a ticket or journey
-- (asked once), those within the cooldown for the rider (asked again after it).
subjectResponses :: RiderState -> DAFC.AppFeedbackConfig -> Text -> [DAFR.AppFeedbackResponse]
subjectResponses st c subjectId =
  [ r
    | r <- st.responses,
      r.configId == c.id,
      r.subjectId == subjectId,
      r.subjectType /= DAFR.RIDER || maybe True (\days -> Time.diffUTCTime st.now r.createdAt < fromIntegral days * 86400) c.cooldownDays
  ]

-- | Whether a response to a question can be stored at a placement, for a question not yet started on its
-- subject (one already started can always be finished). 'reached': opened by an earlier answer as a
-- follow-up, so the logic, placement and subject checks were already passed by its parent.
decideAtPlacement ::
  RiderState ->
  -- | Top-level questions the APP-FEEDBACK logic selected at this placement.
  HS.HashSet (Id DAFC.AppFeedbackConfig) ->
  AppFeedbackContext ->
  LFT.FeedbackResponseStatus ->
  Bool ->
  DAFC.AppFeedbackConfig ->
  Either Text ()
decideAtPlacement st eligibleTop ctx status reached c
  | reached = Right ()
  | isFollowUpOnly c || not (HS.member c.id eligibleTop) = Left "QUESTION_NOT_SERVED"
  | not subjectMatches = Left "SUBJECT_NOT_MATCHED"
  | status == LFT.SHOWN && not (fromMaybe False c.bypassCap) && maybe False (> st.now) st.nextAllowedAt = Left "CAP_REACHED"
  | otherwise = Right ()
  where
    subjectMatches = case c.subjectCondition of
      Nothing -> True
      Just cond -> evaluateRule (Just cond) ctx == Right True

isFinal :: LFT.FeedbackResponseStatus -> Bool
isFinal = (/= LFT.SHOWN)

-- | The rider's latest responses read per request: plenty for cooldowns and history (a rider answers a few a week).
maxResponsesRead :: Int
maxResponsesRead = 500
