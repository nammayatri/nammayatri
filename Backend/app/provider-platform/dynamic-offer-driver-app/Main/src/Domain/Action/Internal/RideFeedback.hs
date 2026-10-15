-- | Ride feedback, served to rider-app (BAP) over the internal API, from booking confirmation to
-- drop-off. The rider app talks to rider-app only; rider-app forwards here with this platform's booking
-- id, and the journey (current booking, live ride, stage) is resolved here. Answers are stored here,
-- next to the driver when they are about a ride. Their action rules are evaluated here too, but the actions themselves (tickets, rider
-- notifications, issue reports, …) are returned to rider-app, which runs them.
module Domain.Action.Internal.RideFeedback
  ( RideFeedbackQuestionsRes (..),
    FeedbackQuestionItem (..),
    FeedbackOptionItem (..),
    FeedbackInputConfigRes (..),
    SubmitRideFeedbackReq (..),
    FeedbackResponseItem (..),
    SubmitRideFeedbackRes (..),
    FeedbackSubmitResult (..),
    MatchedFeedbackAction (..),
    ReportActionResultsReq (..),
    RetryableActionsRes (..),
    getRideFeedbackQuestions,
    postRideFeedback,
    postActionResults,
    getRetryableActions,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (nub, sortOn)
import qualified Data.Text as T
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideFeedbackConfig as DRFC
import qualified Domain.Types.RideFeedbackResponse as DRFR
import Environment
import IssueManagement.Common (Translation (..))
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.RideFeedback.Context
import SharedLogic.RideFeedback.Eligibility
import SharedLogic.RideFeedback.Rule (evaluateRule)
import SharedLogic.RideFeedback.Validation (isBookingQuestion, questionStages)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.RideFeedbackResponse as QRFR
import Tools.Error

-------------------------------------------------------------------------------
-- API types (rider-app mirrors these in its BPP client)

data RideFeedbackQuestionsRes = RideFeedbackQuestionsRes
  { serverTime :: UTCTime,
    questions :: [FeedbackQuestionItem],
    followUpQuestions :: [FeedbackQuestionItem]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackQuestionItem = FeedbackQuestionItem
  { questionId :: Id DRFC.RideFeedbackConfig,
    questionKey :: Text,
    questionType :: DRFC.RideFeedbackQuestionType,
    title :: Text,
    description :: Maybe Text,
    options :: Maybe [FeedbackOptionItem],
    inputConfig :: Maybe FeedbackInputConfigRes,
    acknowledgement :: Maybe Text,
    iconUrl :: Maybe Text,
    isSkippable :: Bool,
    showAt :: UTCTime,
    autoDismissAfterSeconds :: Maybe Int,
    uiConfig :: Maybe A.Value
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackOptionItem = FeedbackOptionItem
  { key :: Text,
    label :: Text,
    iconUrl :: Maybe Text,
    nextQuestionKey :: Maybe Text,
    requiresText :: Bool,
    isExclusive :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackInputConfigRes = FeedbackInputConfigRes
  { placeholder :: Maybe Text,
    minLength :: Maybe Int,
    maxLength :: Maybe Int,
    minValue :: Maybe Double,
    maxValue :: Maybe Double,
    step :: Maybe Double,
    minSelections :: Maybe Int,
    maxSelections :: Maybe Int,
    maxFiles :: Maybe Int,
    maxDurationSec :: Maybe Int,
    minLabel :: Maybe Text,
    maxLabel :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype SubmitRideFeedbackReq = SubmitRideFeedbackReq
  { responses :: [FeedbackResponseItem]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | One response: the question, what happened to it, and the answer when ANSWERED. A follow-up's parent
-- is the answer on this ride that opened it, found on the server.
data FeedbackResponseItem = FeedbackResponseItem
  { questionId :: Id DRFC.RideFeedbackConfig,
    status :: DRFR.RideFeedbackResponseStatus,
    answer :: Maybe DRFR.RideFeedbackAnswer
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype SubmitRideFeedbackRes = SubmitRideFeedbackRes
  { results :: [FeedbackSubmitResult]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackSubmitResult = FeedbackSubmitResult
  { questionId :: Id DRFC.RideFeedbackConfig,
    accepted :: Bool,
    errorCode :: Maybe Text,
    -- | Set when accepted: the stored response, so rider-app can attach its action results to it.
    responseId :: Maybe (Id DRFR.RideFeedbackResponse),
    questionKey :: Maybe Text,
    -- | The ride the answer is about; Nothing for booking-stage questions.
    rideId :: Maybe (Id DRide.Ride),
    -- | The answer as stored (trimmed, deduplicated): what the action rules were evaluated on, so
    -- rider-app runs the actions on the same answer.
    answer :: Maybe DRFR.RideFeedbackAnswer,
    -- | Actions whose rule matched this answer, in rule order. rider-app runs them.
    actions :: [MatchedFeedbackAction]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data MatchedFeedbackAction = MatchedFeedbackAction
  { ruleId :: Text,
    actionType :: DRFC.RideFeedbackActionType,
    params :: Maybe A.Value
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Results of actions rider-app ran for one answer. Each replaces the stored result with the same
-- (ruleId, actionType); results for actions not recorded yet are added.
newtype ReportActionResultsReq = ReportActionResultsReq
  { results :: [DRFR.RideFeedbackActionResult]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | What a retry has to run for one answer: its actions that have not succeeded yet.
data RetryableActionsRes = RetryableActionsRes
  { responseId :: Id DRFR.RideFeedbackResponse,
    questionKey :: Text,
    -- | The ride the answer is about; Nothing for booking-stage questions.
    rideId :: Maybe (Id DRide.Ride),
    answer :: DRFR.RideFeedbackAnswer,
    actions :: [MatchedFeedbackAction]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-------------------------------------------------------------------------------
-- GET /internal/booking/{bookingId}/feedback/questions

getRideFeedbackQuestions :: Id DB.Booking -> Maybe Text -> Maybe Language -> Flow RideFeedbackQuestionsRes
getRideFeedbackQuestions bookingId apiKey mbLanguage = do
  journey <- fetchAuthorisedJourney bookingId apiKey
  now <- getCurrentTime
  configs <- cityQuestions journey.booking journey.booking.merchantOperatingCityId (Just True)
  -- Cheap exit for the common case: no question in this city applies to where the journey is now.
  if not (any (stageAllowed journey) configs)
    then pure $ emptyQuestionsRes now
    else do
      evaluation <- evaluateJourney now journey configs
      let language = fromMaybe ENGLISH mbLanguage
          eligibleTop = [(c, t) | (c, Eligible t) <- evaluation.decisions]
      pure
        RideFeedbackQuestionsRes
          { serverTime = now,
            questions = [mkQuestionItem language t c | (c, t) <- sortOn (priorityKey . fst) eligibleTop],
            followUpQuestions = map (mkQuestionItem language now) evaluation.followUps
          }

mkQuestionItem :: Language -> UTCTime -> DRFC.RideFeedbackConfig -> FeedbackQuestionItem
mkQuestionItem language showAt config =
  FeedbackQuestionItem
    { questionId = config.id,
      questionKey = config.questionKey,
      questionType = config.questionType,
      title = fromMaybe config.questionKey (pick $ Just config.title),
      description = pick config.description,
      options = map mkOption <$> config.options,
      inputConfig = mkInputConfig <$> config.inputConfig,
      acknowledgement = pick config.acknowledgement,
      iconUrl = config.iconUrl,
      isSkippable = fromMaybe True config.isSkippable,
      showAt,
      autoDismissAfterSeconds = config.displayTrigger >>= (.autoDismissAfterSeconds),
      uiConfig = config.uiConfig
    }
  where
    pick = (>>= pickTranslation language)
    mkOption o =
      FeedbackOptionItem
        { key = o.key,
          label = fromMaybe o.key (pickTranslation language o.label),
          iconUrl = o.iconUrl,
          nextQuestionKey = o.nextQuestionKey,
          requiresText = fromMaybe False o.requiresText,
          isExclusive = fromMaybe False o.isExclusive
        }
    mkInputConfig ic =
      FeedbackInputConfigRes
        { placeholder = pick ic.placeholder,
          minLength = ic.minLength,
          maxLength = ic.maxLength,
          minValue = ic.minValue,
          maxValue = ic.maxValue,
          step = ic.step,
          minSelections = ic.minSelections,
          maxSelections = ic.maxSelections,
          maxFiles = ic.maxFiles,
          maxDurationSec = ic.maxDurationSec,
          minLabel = pick ic.minLabel,
          maxLabel = pick ic.maxLabel
        }

-------------------------------------------------------------------------------
-- POST /internal/booking/{bookingId}/feedback

postRideFeedback :: Id DB.Booking -> Maybe Text -> SubmitRideFeedbackReq -> Flow SubmitRideFeedbackRes
postRideFeedback bookingId apiKey req = do
  when (length req.responses > maxResponsesPerRequest) $
    throwError $ InvalidRequest ("At most " <> show maxResponsesPerRequest <> " responses per request")
  journey <- fetchAuthorisedJourney bookingId apiKey
  let booking = journey.booking
  configs <- cityQuestions booking booking.merchantOperatingCityId (Just True)
  configPilotVersions <- servedConfigVersions booking.merchantOperatingCityId
  now <- getCurrentTime
  let configById = HM.fromList [(c.id, c) | c <- configs]
  -- One eligibility pass gives the logic version (stable per rider, saved for A/B analysis), the
  -- questions this journey may be served, and the context action rules are evaluated against.
  evaluation <- evaluateJourney now journey configs
  let logicVersion = evaluation.logicVersion
      ruleContext = evaluation.context
      eligibleTop = HS.fromList [c.id | (c, Eligible _) <- evaluation.decisions]
  results <-
    Hedis.withLockRedisAndReturnValue (submitLockKey journey.transactionId) submitLockSeconds $ do
      existing <- QRFR.findAllByTransactionId journey.transactionId
      let initial = currentResponses journey existing
          env = SubmitEnv {..}
      (acc, _) <- foldM (\(results', seenResponses) item -> first (: results') <$> submitItem env seenResponses item) ([], initial) req.responses
      pure $ reverse acc
  pure SubmitRideFeedbackRes {results}

data SubmitEnv = SubmitEnv
  { now :: UTCTime,
    journey :: Journey,
    configById :: HM.HashMap (Id DRFC.RideFeedbackConfig) DRFC.RideFeedbackConfig,
    logicVersion :: Maybe Int,
    -- | Config Pilot versions the questions were served from (stored for A/B analysis of staged changes).
    configPilotVersions :: [Int],
    -- | Nothing while the journey has no stage (only already-shown questions can then be finished).
    ruleContext :: Maybe RideFeedbackContext,
    -- | Top-level questions the RIDE-FEEDBACK logic selected for this journey and that are still open.
    eligibleTop :: HS.HashSet (Id DRFC.RideFeedbackConfig)
  }

type SubmitState = HM.HashMap (Id DRFC.RideFeedbackConfig) DRFR.RideFeedbackResponse

submitItem :: SubmitEnv -> SubmitState -> FeedbackResponseItem -> Flow (FeedbackSubmitResult, SubmitState)
submitItem env seenResponses item =
  case HM.lookup item.questionId env.configById of
    Nothing -> rejected "QUESTION_NOT_FOUND"
    Just config
      | isNothing (subjectOf env.journey config) -> rejected "NO_RIDE"
      | not (submissionAllowed env.journey seenResponses config) -> rejected "STAGE_NOT_ALLOWED"
      -- A client can only start a question this journey was served: one the logic selected, or a
      -- follow-up opened by an earlier answer. Its action rules can raise tickets and escalations.
      | not (HM.member config.id seenResponses),
        not (HS.member config.id env.eligibleTop && not (isFollowUpOnly config)),
        not (followUpReached env.configById seenResponses config) ->
        rejected "QUESTION_NOT_SERVED"
      | Just prev <- HM.lookup config.id seenResponses, isFinal prev.status -> rejected "ALREADY_ANSWERED"
      | item.status == DRFR.ANSWERED -> case validateAnswer config item.answer of
        Left code -> rejected code
        Right answer -> do
          saved <- upsertResponse env seenResponses config item (Just answer)
          actions <- maybe (pure []) (matchActionRules config . withAnswer answer) env.ruleContext
          -- Recorded before rider-app runs them, so a lost request still leaves an audit trail.
          saved' <-
            if null actions
              then pure saved
              else do
                let pending = map (pendingResult env.now) actions
                QRFR.updateActionResults (Just pending) saved.id
                pure saved {DRFR.actionResults = Just pending}
          accepted config saved' actions
      | otherwise -> do
        saved <- upsertResponse env seenResponses config item Nothing
        accepted config saved []
  where
    rejected code =
      pure
        ( FeedbackSubmitResult {questionId = item.questionId, accepted = False, errorCode = Just code, responseId = Nothing, questionKey = Nothing, rideId = Nothing, answer = Nothing, actions = []},
          seenResponses
        )
    accepted config saved actions =
      pure
        ( FeedbackSubmitResult {questionId = item.questionId, accepted = True, errorCode = Nothing, responseId = Just saved.id, questionKey = Just config.questionKey, rideId = saved.rideId, answer = saved.answer, actions},
          HM.insert config.id saved seenResponses
        )

-- | Actions of every rule whose condition matches the answer. A rule that fails to evaluate is
-- skipped and logged, never matched by accident.
matchActionRules :: DRFC.RideFeedbackConfig -> RideFeedbackContext -> Flow [MatchedFeedbackAction]
matchActionRules config ctx = concat <$> mapM matchRule (fromMaybe [] config.actionRules)
  where
    matchRule rule = case evaluateRule rule.condition ctx of
      Right True -> pure [MatchedFeedbackAction {ruleId = rule.ruleId, actionType = a.actionType, params = a.params} | a <- rule.actions]
      Right False -> pure []
      Left err -> do
        logError $ "RideFeedback: skipping action rule " <> rule.ruleId <> " of " <> config.questionKey <> ": " <> err
        pure []

upsertResponse :: SubmitEnv -> SubmitState -> DRFC.RideFeedbackConfig -> FeedbackResponseItem -> Maybe DRFR.RideFeedbackAnswer -> Flow DRFR.RideFeedbackResponse
upsertResponse env seenResponses config item answer = do
  let journey = env.journey
      -- A booking-stage answer is about the booking, not whichever driver is assigned by then.
      ride = if isBookingQuestion config then Nothing else journey.ride
      secondsSinceBooking = floor (diffUTCTime env.now journey.bookedAt)
      secondsIntoRide = (\t -> floor (diffUTCTime env.now t)) <$> (ride >>= (.tripStartTime))
      nonEmptyVersions = if null env.configPilotVersions then Nothing else Just env.configPilotVersions
  case HM.lookup config.id seenResponses of
    Just prev -> do
      let updated =
            (prev :: DRFR.RideFeedbackResponse)
              { DRFR.status = item.status,
                DRFR.answer = answer <|> prev.answer,
                DRFR.answerScore = maybe prev.answerScore (answerScore config) answer,
                DRFR.logicVersion = prev.logicVersion <|> env.logicVersion,
                DRFR.configPilotVersions = prev.configPilotVersions <|> nonEmptyVersions,
                DRFR.secondsSinceBooking = secondsSinceBooking,
                DRFR.secondsIntoRide = secondsIntoRide,
                DRFR.updatedAt = env.now
              }
      QRFR.updateByPrimaryKey updated
      pure updated
    Nothing -> do
      responseId <- generateGUID
      subjectId <- subjectOf journey config & fromMaybeM (InvalidRequest "This question needs a ride")
      let created =
            DRFR.RideFeedbackResponse
              { id = responseId,
                transactionId = journey.transactionId,
                bookingId = journey.booking.id,
                rideId = (.id) <$> ride,
                driverId = (.driverId) <$> ride,
                subjectId,
                merchantId = journey.booking.providerId,
                merchantOperatingCityId = journey.booking.merchantOperatingCityId,
                configId = config.id,
                questionKey = config.questionKey,
                questionType = config.questionType,
                logicVersion = env.logicVersion,
                configPilotVersions = nonEmptyVersions,
                status = item.status,
                answer,
                answerScore = answerScore config =<< answer,
                stage = fromMaybe (fromMaybe DRFC.RIDE_IN_PROGRESS (listToMaybe (questionStages config))) journey.stage,
                secondsSinceBooking,
                secondsIntoRide,
                actionResults = Nothing,
                createdAt = env.now,
                updatedAt = env.now
              }
      QRFR.create created
      pure created

-- | Returns the answer to store, or an error code for the client. Only the parts of an answer that belong
-- to the question's type are accepted (anything else is rejected, not dropped, so a client bug shows
-- up), and option keys must be the question's own.
validateAnswer :: DRFC.RideFeedbackConfig -> Maybe DRFR.RideFeedbackAnswer -> Either Text DRFR.RideFeedbackAnswer
validateAnswer config = \case
  Nothing -> Left "ANSWER_REQUIRED"
  Just ans -> do
    let selectedKeys = nub $ fromMaybe [] ans.selectedOptionKeys
        options = fromMaybe [] config.options
        optionKeys = map (.key) options
        picked = [o | o <- options, o.key `elem` selectedKeys]
        exclusiveKeys = [o.key | o <- options, o.isExclusive == Just True]
        ic = config.inputConfig
        inputInt f = ic >>= f
        inputDouble f = ic >>= f
        text = mfilter (not . T.null) (T.strip <$> ans.text)
        textLen = maybe 0 T.length text
        files = fromMaybe [] ans.mediaFileIds
        withinD lo hi v = maybe True (v >=) lo && maybe True (v <=) hi
        -- A comment is part of a choice answer only when a picked option asks for one.
        commentWanted = any ((== Just True) . (.requiresText)) picked
        -- The parts of an answer (options, text, number, rating, media) the question's type takes.
        only :: [Text] -> Either Text ()
        only fields =
          check
            ( and
                [ "options" `elem` fields || null selectedKeys,
                  "text" `elem` fields || isNothing text,
                  "number" `elem` fields || isNothing ans.number,
                  "rating" `elem` fields || isNothing ans.rating,
                  "media" `elem` fields || null files
                ]
            )
            "UNEXPECTED_FIELD"
        onStep v = maybe True (\st -> let n = (v - fromMaybe 0 (inputDouble (.minValue))) / st in abs (n - fromIntegral (round n :: Int)) < 1e-9) (inputDouble (.step))
        choice = do
          check (all (`elem` optionKeys) selectedKeys) "INVALID_OPTION"
          only (["options"] <> ["text" | commentWanted])
          when commentWanted $ check (textLen > 0) "COMMENT_REQUIRED"
    check (textLen <= fromMaybe defaultMaxTextLength (inputInt (.maxLength))) "TEXT_TOO_LONG"
    case config.questionType of
      t
        | t `elem` [DRFC.SINGLE_SELECT, DRFC.YES_NO, DRFC.THUMBS, DRFC.EMOJI_SCALE] -> do
          check (length selectedKeys == 1) "INVALID_OPTION"
          choice
      DRFC.MULTI_SELECT -> do
        check (not (null selectedKeys)) "INVALID_OPTION"
        choice
        check (length selectedKeys >= fromMaybe 1 (inputInt (.minSelections)) && maybe True (length selectedKeys <=) (inputInt (.maxSelections))) "INVALID_SELECTION_COUNT"
        check (not (any (`elem` exclusiveKeys) selectedKeys) || length selectedKeys == 1) "EXCLUSIVE_OPTION_COMBINED"
      DRFC.STAR_RATING -> do
        only ["rating"]
        check (maybe False (withinD (Just $ fromMaybe 1 (inputDouble (.minValue))) (Just $ fromMaybe 5 (inputDouble (.maxValue))) . fromIntegral) ans.rating) "INVALID_RATING"
      t
        | t `elem` [DRFC.SCALE, DRFC.NUMBER_INPUT] -> do
          only ["number"]
          check (maybe False (\v -> withinD (inputDouble (.minValue)) (inputDouble (.maxValue)) v && onStep v) ans.number) "INVALID_NUMBER"
      DRFC.TEXT_INPUT -> do
        only ["text"]
        check (textLen > 0 && textLen >= fromMaybe 0 (inputInt (.minLength))) "INVALID_TEXT"
      t
        | t `elem` [DRFC.IMAGE_UPLOAD, DRFC.AUDIO] -> do
          only ["media"]
          check (not (null files) && length files <= fromMaybe defaultMaxFiles (inputInt (.maxFiles))) "INVALID_MEDIA"
      _ -> pure ()
    pure
      DRFR.RideFeedbackAnswer
        { selectedOptionKeys = if null selectedKeys then Nothing else Just selectedKeys,
          text,
          number = ans.number,
          rating = ans.rating,
          mediaFileIds = if null files then Nothing else Just files
        }
  where
    check cond code = unless cond (Left code)

-- | 0 (bad) to 1 (good), comparable across questions, so driver behaviour can aggregate answers without
-- knowing what each question means: a rating or number placed within the question's range, or the
-- configured score of the picked option (the lowest one for a multi-select). Nothing when the
-- question is not scored (text, media, options without scores, a number without both bounds).
answerScore :: DRFC.RideFeedbackConfig -> DRFR.RideFeedbackAnswer -> Maybe Double
answerScore config ans = case config.questionType of
  DRFC.STAR_RATING -> placed (Just $ fromMaybe 1 (ic >>= (.minValue))) (Just $ fromMaybe 5 (ic >>= (.maxValue))) (fromIntegral <$> ans.rating)
  DRFC.SCALE -> placed (ic >>= (.minValue)) (ic >>= (.maxValue)) ans.number
  DRFC.NUMBER_INPUT -> placed (ic >>= (.minValue)) (ic >>= (.maxValue)) ans.number
  _ ->
    let picked = fromMaybe [] ans.selectedOptionKeys
        scores = [s | o <- fromMaybe [] config.options, o.key `elem` picked, Just s <- [o.score]]
     in if null scores then Nothing else Just (minimum scores)
  where
    ic = config.inputConfig
    placed mbLo mbHi mbV = do
      lo <- mbLo
      hi <- mbHi
      v <- mbV
      guard (hi > lo)
      pure $ max 0 (min 1 ((v - lo) / (hi - lo)))

-------------------------------------------------------------------------------
-- POST /internal/booking/{bookingId}/feedback/response/{responseId}/actionResults

postActionResults :: Id DB.Booking -> Id DRFR.RideFeedbackResponse -> Maybe Text -> ReportActionResultsReq -> Flow APISuccess
postActionResults bookingId responseId apiKey req = do
  journey <- fetchAuthorisedJourney bookingId apiKey
  -- Read-merge-write under the journey's lock: the first run and a retry can report at the same time.
  Hedis.withLockRedisAndReturnValue (submitLockKey journey.transactionId) submitLockSeconds $ do
    response <- fetchJourneyResponse journey responseId
    let previous = fromMaybe [] response.actionResults
        -- A retry reports its own run; attempts add up so the inspector shows every try.
        updated = [maybe r (\new -> (new :: DRFR.RideFeedbackActionResult) {DRFR.attempts = r.attempts + new.attempts}) (find (sameAction r) req.results) | r <- previous]
        added = [r | r <- req.results, not (any (sameAction r) previous)]
    QRFR.updateActionResults (Just $ updated <> added) response.id
  pure Success

-------------------------------------------------------------------------------
-- GET /internal/booking/{bookingId}/feedback/response/{responseId}/retryableActions

-- | Actions of the answer that failed, or are still PENDING long after they started (their run was lost),
-- so a retry never repeats a ticket or report that went through or may still be going through. The
-- actions handed out are marked PENDING again under the ride's lock, so two retries do not both run
-- them. Actions whose rule was since removed from the question are not returned.
-- The question is read as this journey is served it (Config Pilot, pinned to its transaction).
getRetryableActions :: Id DB.Booking -> Id DRFR.RideFeedbackResponse -> Maybe Text -> Flow RetryableActionsRes
getRetryableActions bookingId responseId apiKey = do
  journey <- fetchAuthorisedJourney bookingId apiKey
  let booking = journey.booking
  response <- fetchJourneyResponse journey responseId
  answer <- case (response.status, response.answer) of
    (DRFR.ANSWERED, Just a) -> pure a
    _ -> throwError (InvalidRequest "Only answered responses have actions")
  config <- find ((== response.configId) . (.id)) <$> cityQuestions booking booking.merchantOperatingCityId Nothing >>= fromMaybeM (RideFeedbackConfigNotFound response.configId.getId)
  now <- getCurrentTime
  Hedis.withLockRedisAndReturnValue (submitLockKey journey.transactionId) submitLockSeconds $ do
    current <- fetchJourneyResponse journey responseId
    let results = fromMaybe [] current.actionResults
        retryable r = r.status == DRFR.FAILED || (r.status == DRFR.PENDING && diffUTCTime now r.updatedAt > stalePendingSeconds)
        due = filter retryable results
        actions =
          [ MatchedFeedbackAction {ruleId = rule.ruleId, actionType = action.actionType, params = action.params}
            | rule <- fromMaybe [] config.actionRules,
              action <- rule.actions,
              any (\r -> r.ruleId == rule.ruleId && r.actionType == action.actionType) due
          ]
        claimed = [if any (sameAction r) (map (pendingResult now) actions) then (r :: DRFR.RideFeedbackActionResult) {DRFR.status = DRFR.PENDING, DRFR.updatedAt = now} else r | r <- results]
    unless (null actions) $ QRFR.updateActionResults (Just claimed) current.id
    pure RetryableActionsRes {responseId = current.id, questionKey = current.questionKey, rideId = current.rideId, answer, actions}

-------------------------------------------------------------------------------
-- Helpers

fetchJourneyResponse :: Journey -> Id DRFR.RideFeedbackResponse -> Flow DRFR.RideFeedbackResponse
fetchJourneyResponse journey responseId = do
  response <- QRFR.findByPrimaryKey responseId >>= fromMaybeM (InvalidRequest $ "Ride feedback response " <> responseId.getId <> " not found")
  unless (response.transactionId == journey.transactionId) $ throwError (InvalidRequest $ "Ride feedback response " <> responseId.getId <> " not found")
  pure response

pendingResult :: UTCTime -> MatchedFeedbackAction -> DRFR.RideFeedbackActionResult
pendingResult now action =
  DRFR.RideFeedbackActionResult
    { ruleId = action.ruleId,
      actionType = action.actionType,
      status = DRFR.PENDING,
      attempts = 0,
      externalRef = Nothing,
      errorMessage = Nothing,
      updatedAt = now
    }

-- | The booking's journey, after checking the caller holds the merchant's internal API key. Any booking of
-- the journey works: rider-app may still hold the one a reallocation replaced.
fetchAuthorisedJourney :: Id DB.Booking -> Maybe Text -> Flow Journey
fetchAuthorisedJourney bookingId apiKey = do
  booking <- QB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  merchant <- CQM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  resolveJourney booking

pickTranslation :: Language -> [Translation] -> Maybe Text
pickTranslation language translations =
  (.translation)
    <$> ( find ((== language) . (.language)) translations
            <|> find ((== ENGLISH) . (.language)) translations
            <|> listToMaybe translations
        )

-- | A response is accepted while the journey is in one of the question's stages. A question already shown
-- on its subject can still be finished after the stage has moved on: the rider app hears of a driver
-- assignment or the ride's end a few seconds after this platform, so a tap can land just after it.
submissionAllowed :: Journey -> SubmitState -> DRFC.RideFeedbackConfig -> Bool
submissionAllowed journey seenResponses config =
  stageAllowed journey config || HM.member config.id seenResponses

-- | Whether an answer already given on this ride (stored, or earlier in the same request) picked an
-- option that opens this question.
followUpReached :: HM.HashMap (Id DRFC.RideFeedbackConfig) DRFC.RideFeedbackConfig -> SubmitState -> DRFC.RideFeedbackConfig -> Bool
followUpReached configById seenResponses config =
  or
    [ True
      | r <- HM.elems seenResponses,
        r.status == DRFR.ANSWERED,
        Just parent <- [HM.lookup r.configId configById],
        o <- fromMaybe [] parent.options,
        o.nextQuestionKey == Just config.questionKey,
        o.key `elem` fromMaybe [] (r.answer >>= (.selectedOptionKeys))
    ]

emptyQuestionsRes :: UTCTime -> RideFeedbackQuestionsRes
emptyQuestionsRes now = RideFeedbackQuestionsRes {serverTime = now, questions = [], followUpQuestions = []}

-- | Same rule and action type: one entry of a response's action results.
sameAction :: DRFR.RideFeedbackActionResult -> DRFR.RideFeedbackActionResult -> Bool
sameAction a b = a.ruleId == b.ruleId && a.actionType == b.actionType

-- | A PENDING action older than this is taken as lost (rider-app's run finishes well within it).
stalePendingSeconds :: NominalDiffTime
stalePendingSeconds = 300

submitLockKey :: Text -> Text
submitLockKey transactionId = "RideFeedback:SubmitLock:TransactionId-" <> transactionId

submitLockSeconds :: Hedis.ExpirationTime
submitLockSeconds = 10

maxResponsesPerRequest :: Int
maxResponsesPerRequest = 20

defaultMaxTextLength :: Int
defaultMaxTextLength = 1000

defaultMaxFiles :: Int
defaultMaxFiles = 3
