-- | During-ride feedback, served to rider-app (BAP) over the internal API. The rider app talks to
-- rider-app only; rider-app forwards here with this ride's id. Answers are stored here, next to the
-- driver. Their action rules are evaluated here too, but the actions themselves (tickets, rider
-- notifications, issue reports, …) are returned to rider-app, which runs them.
module Domain.Action.Internal.RideFeedback
  ( RideFeedbackQuestionsRes (..),
    FeedbackQuestionItem (..),
    FeedbackOptionItem (..),
    FeedbackInputConfigRes (..),
    SubmitRideFeedbackReq (..),
    FeedbackResponseItem (..),
    FeedbackLocation (..),
    SubmitRideFeedbackRes (..),
    FeedbackSubmitResult (..),
    MatchedFeedbackAction (..),
    RideFeedbackSubmittedRes (..),
    SubmittedFeedbackItem (..),
    ReportActionResultsReq (..),
    RetryableActionsRes (..),
    getRideFeedbackQuestions,
    postRideFeedback,
    getRideFeedback,
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
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideFeedbackResponse as QRFR
import Tools.Error

-------------------------------------------------------------------------------
-- API types (rider-app mirrors these in its BPP client)

data RideFeedbackQuestionsRes = RideFeedbackQuestionsRes
  { rideId :: Id DRide.Ride,
    serverTime :: UTCTime,
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
    isSkippable :: Bool,
    showAt :: UTCTime,
    autoDismissAfterSeconds :: Maybe Int,
    priority :: Int,
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

data FeedbackResponseItem = FeedbackResponseItem
  { questionId :: Id DRFC.RideFeedbackConfig,
    status :: DRFR.RideFeedbackResponseStatus,
    answer :: Maybe DRFR.RideFeedbackAnswer,
    parentQuestionId :: Maybe (Id DRFC.RideFeedbackConfig),
    clientTimestamp :: Maybe UTCTime,
    location :: Maybe FeedbackLocation
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data FeedbackLocation = FeedbackLocation
  { lat :: Double,
    lon :: Double
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
    acknowledgement :: Maybe Text,
    -- | Set when accepted: the stored response, so rider-app can attach its action results to it.
    responseId :: Maybe (Id DRFR.RideFeedbackResponse),
    questionKey :: Maybe Text,
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

newtype RideFeedbackSubmittedRes = RideFeedbackSubmittedRes
  { responses :: [SubmittedFeedbackItem]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SubmittedFeedbackItem = SubmittedFeedbackItem
  { responseId :: Id DRFR.RideFeedbackResponse,
    questionId :: Id DRFC.RideFeedbackConfig,
    questionKey :: Text,
    status :: DRFR.RideFeedbackResponseStatus,
    answer :: Maybe DRFR.RideFeedbackAnswer,
    updatedAt :: UTCTime
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
    answer :: DRFR.RideFeedbackAnswer,
    actions :: [MatchedFeedbackAction]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-------------------------------------------------------------------------------
-- GET /internal/ride/{rideId}/feedback/questions

getRideFeedbackQuestions :: Id DRide.Ride -> Maybe Text -> Maybe Language -> Flow RideFeedbackQuestionsRes
getRideFeedbackQuestions rideId apiKey mbLanguage = do
  (ride, booking) <- fetchAuthorisedRide rideId apiKey
  now <- getCurrentTime
  configs <- cityQuestions booking ride.merchantOperatingCityId (Just True)
  -- Cheap exit for the common case: no question in this city applies to the ride's current status.
  if not (any (statusAllowed ride.status) configs)
    then pure $ emptyQuestionsRes rideId now
    else do
      evaluation <- evaluateRide now ride booking configs
      let language = fromMaybe ENGLISH mbLanguage
          eligibleTop = [(c, t) | (c, Eligible t) <- evaluation.decisions]
      pure
        RideFeedbackQuestionsRes
          { rideId,
            serverTime = now,
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
      isSkippable = fromMaybe True config.isSkippable,
      showAt,
      autoDismissAfterSeconds = config.displayTrigger >>= (.autoDismissAfterSeconds),
      priority = fromMaybe defaultPriority config.priority,
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
-- POST /internal/ride/{rideId}/feedback

postRideFeedback :: Id DRide.Ride -> Maybe Text -> Maybe Language -> SubmitRideFeedbackReq -> Flow SubmitRideFeedbackRes
postRideFeedback rideId apiKey mbLanguage req = do
  when (length req.responses > maxResponsesPerRequest) $
    throwError $ InvalidRequest ("At most " <> show maxResponsesPerRequest <> " responses per request")
  (ride, booking) <- fetchAuthorisedRide rideId apiKey
  configs <- cityQuestions booking ride.merchantOperatingCityId (Just True)
  configPilotVersions <- servedConfigVersions ride.merchantOperatingCityId
  now <- getCurrentTime
  let language = fromMaybe ENGLISH mbLanguage
      configById = HM.fromList [(c.id, c) | c <- configs]
  -- One eligibility pass gives the logic version (stable per rider, saved for A/B analysis), the
  -- questions this ride may be served, and the context action rules are evaluated against.
  evaluation <- evaluateRide now ride booking configs
  let logicVersion = evaluation.logicVersion
      ruleContext = evaluation.context
      eligibleTop = HS.fromList [c.id | (c, Eligible _) <- evaluation.decisions]
  results <-
    Hedis.withLockRedisAndReturnValue (submitLockKey rideId) submitLockSeconds $ do
      existing <- QRFR.findAllByRideId ride.id
      let initial = HM.fromList [(r.configId, r) | r <- existing]
          env = SubmitEnv {..}
      (acc, _) <- foldM (\(results', seenResponses) item -> first (: results') <$> submitItem env seenResponses item) ([], initial) req.responses
      pure $ reverse acc
  pure SubmitRideFeedbackRes {results}

data SubmitEnv = SubmitEnv
  { now :: UTCTime,
    language :: Language,
    ride :: DRide.Ride,
    booking :: DB.Booking,
    configById :: HM.HashMap (Id DRFC.RideFeedbackConfig) DRFC.RideFeedbackConfig,
    logicVersion :: Maybe Int,
    -- | Config Pilot versions the questions were served from (stored for A/B analysis of staged changes).
    configPilotVersions :: [Int],
    ruleContext :: RideFeedbackContext,
    -- | Top-level questions the RIDE-FEEDBACK logic selected for this ride and that are still open.
    eligibleTop :: HS.HashSet (Id DRFC.RideFeedbackConfig)
  }

type SubmitState = HM.HashMap (Id DRFC.RideFeedbackConfig) DRFR.RideFeedbackResponse

submitItem :: SubmitEnv -> SubmitState -> FeedbackResponseItem -> Flow (FeedbackSubmitResult, SubmitState)
submitItem env seenResponses item =
  case HM.lookup item.questionId env.configById of
    Nothing -> rejected "QUESTION_NOT_FOUND"
    Just config
      | not (submissionAllowed env.ride seenResponses config) -> rejected "RIDE_STATUS_NOT_ALLOWED"
      -- A client can only start a question this ride was served: one the logic selected, or a
      -- follow-up opened by an answer on this ride. Its action rules can raise tickets and escalations.
      | not (HM.member config.id seenResponses),
        not (HS.member config.id env.eligibleTop && not (isFollowUpOnly config)),
        not (followUpReached env.configById seenResponses config) ->
        rejected "QUESTION_NOT_SERVED"
      | Just prev <- HM.lookup config.id seenResponses, isFinal prev.status -> rejected "ALREADY_ANSWERED"
      | item.status == DRFR.ANSWERED -> case validateAnswer config item.answer of
        Left code -> rejected code
        Right answer -> do
          saved <- upsertResponse env seenResponses config item (Just answer)
          actions <- matchActionRules config (withAnswer answer env.ruleContext)
          -- Recorded before rider-app runs them, so a lost request still leaves an audit trail.
          saved' <-
            if null actions
              then pure saved
              else do
                let pending = map (pendingResult env.now) actions
                QRFR.updateActionResults (Just pending) saved.id
                pure saved {DRFR.actionResults = Just pending}
          accepted config saved' (config.acknowledgement >>= pickTranslation env.language) actions
      | otherwise -> do
        saved <- upsertResponse env seenResponses config item Nothing
        accepted config saved Nothing []
  where
    rejected code =
      pure
        ( FeedbackSubmitResult {questionId = item.questionId, accepted = False, errorCode = Just code, acknowledgement = Nothing, responseId = Nothing, questionKey = Nothing, actions = []},
          seenResponses
        )
    accepted config saved acknowledgement actions =
      pure
        ( FeedbackSubmitResult {questionId = item.questionId, accepted = True, errorCode = Nothing, acknowledgement, responseId = Just saved.id, questionKey = Just config.questionKey, actions},
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
  let parentResponseId = item.parentQuestionId >>= (`HM.lookup` seenResponses) <&> (.id)
      secondsIntoRide = (\t -> floor (diffUTCTime env.now t)) <$> env.ride.tripStartTime
      nonEmptyVersions = if null env.configPilotVersions then Nothing else Just env.configPilotVersions
  case HM.lookup config.id seenResponses of
    Just prev -> do
      let updated =
            (prev :: DRFR.RideFeedbackResponse)
              { DRFR.status = item.status,
                DRFR.answer = answer <|> prev.answer,
                DRFR.logicVersion = prev.logicVersion <|> env.logicVersion,
                DRFR.configPilotVersions = prev.configPilotVersions <|> nonEmptyVersions,
                DRFR.parentResponseId = parentResponseId <|> prev.parentResponseId,
                DRFR.rideStatusAtResponse = Just env.ride.status,
                DRFR.secondsIntoRide = secondsIntoRide,
                DRFR.lat = (item.location <&> (.lat)) <|> prev.lat,
                DRFR.lon = (item.location <&> (.lon)) <|> prev.lon,
                DRFR.updatedAt = env.now
              }
      QRFR.updateByPrimaryKey updated
      pure updated
    Nothing -> do
      responseId <- generateGUID
      let created =
            DRFR.RideFeedbackResponse
              { id = responseId,
                rideId = env.ride.id,
                bookingId = env.booking.id,
                driverId = env.ride.driverId,
                merchantId = env.booking.providerId,
                merchantOperatingCityId = env.ride.merchantOperatingCityId,
                configId = config.id,
                questionKey = config.questionKey,
                logicVersion = env.logicVersion,
                configPilotVersions = nonEmptyVersions,
                parentResponseId,
                status = item.status,
                answer,
                rideStatusAtResponse = Just env.ride.status,
                secondsIntoRide,
                lat = item.location <&> (.lat),
                lon = item.location <&> (.lon),
                actionResults = Nothing,
                createdAt = env.now,
                updatedAt = env.now
              }
      QRFR.create created
      pure created

-- | Returns the answer to store (option keys de-duplicated) or an error code for the client.
validateAnswer :: DRFC.RideFeedbackConfig -> Maybe DRFR.RideFeedbackAnswer -> Either Text DRFR.RideFeedbackAnswer
validateAnswer config = \case
  Nothing -> Left "ANSWER_REQUIRED"
  Just ans -> do
    let selectedKeys = nub $ fromMaybe [] ans.selectedOptionKeys
        options = fromMaybe [] config.options
        optionKeys = map (.key) options
        exclusiveKeys = [o.key | o <- options, o.isExclusive == Just True]
        ic = config.inputConfig
        inputInt f = ic >>= f
        inputDouble f = ic >>= f
        textLen = maybe 0 (T.length . T.strip) ans.text
        knownKeys = null optionKeys || all (`elem` optionKeys) selectedKeys
        withinD lo hi v = maybe True (v >=) lo && maybe True (v <=) hi
    check (textLen <= fromMaybe defaultMaxTextLength (inputInt (.maxLength))) "TEXT_TOO_LONG"
    case config.questionType of
      t
        | t `elem` [DRFC.SINGLE_SELECT, DRFC.YES_NO, DRFC.THUMBS, DRFC.EMOJI_SCALE] ->
          check (length selectedKeys == 1 && knownKeys) "INVALID_OPTION"
      DRFC.MULTI_SELECT -> do
        check (not (null selectedKeys) && knownKeys) "INVALID_OPTION"
        check (length selectedKeys >= fromMaybe 1 (inputInt (.minSelections)) && maybe True (length selectedKeys <=) (inputInt (.maxSelections))) "INVALID_SELECTION_COUNT"
        check (not (any (`elem` exclusiveKeys) selectedKeys) || length selectedKeys == 1) "EXCLUSIVE_OPTION_COMBINED"
      DRFC.STAR_RATING ->
        check (maybe False (withinD (Just $ fromMaybe 1 (inputDouble (.minValue))) (Just $ fromMaybe 5 (inputDouble (.maxValue))) . fromIntegral) ans.rating) "INVALID_RATING"
      t
        | t `elem` [DRFC.SCALE, DRFC.NUMBER_INPUT] ->
          check (maybe False (withinD (inputDouble (.minValue)) (inputDouble (.maxValue))) ans.number) "INVALID_NUMBER"
      DRFC.TEXT_INPUT ->
        check (textLen > 0 && textLen >= fromMaybe 0 (inputInt (.minLength))) "INVALID_TEXT"
      t
        | t `elem` [DRFC.IMAGE_UPLOAD, DRFC.AUDIO] ->
          let files = fromMaybe [] ans.mediaFileIds
           in check (not (null files) && length files <= fromMaybe defaultMaxFiles (inputInt (.maxFiles))) "INVALID_MEDIA"
      _ -> pure ()
    pure (ans :: DRFR.RideFeedbackAnswer) {DRFR.selectedOptionKeys = if null selectedKeys then Nothing else Just selectedKeys}
  where
    check cond code = unless cond (Left code)

-------------------------------------------------------------------------------
-- GET /internal/ride/{rideId}/feedback

getRideFeedback :: Id DRide.Ride -> Maybe Text -> Flow RideFeedbackSubmittedRes
getRideFeedback rideId apiKey = do
  void $ fetchAuthorisedRide rideId apiKey
  responses <- QRFR.findAllByRideId rideId
  pure
    RideFeedbackSubmittedRes
      { responses =
          [ SubmittedFeedbackItem
              { responseId = r.id,
                questionId = r.configId,
                questionKey = r.questionKey,
                status = r.status,
                answer = r.answer,
                updatedAt = r.updatedAt
              }
            | r <- sortOn (.createdAt) responses
          ]
      }

-------------------------------------------------------------------------------
-- POST /internal/ride/{rideId}/feedback/response/{responseId}/actionResults

postActionResults :: Id DRide.Ride -> Id DRFR.RideFeedbackResponse -> Maybe Text -> ReportActionResultsReq -> Flow APISuccess
postActionResults rideId responseId apiKey req = do
  void $ fetchAuthorisedRide rideId apiKey
  -- Read-merge-write under the ride's lock: the first run and a retry can report at the same time.
  Hedis.withLockRedisAndReturnValue (submitLockKey rideId) submitLockSeconds $ do
    response <- fetchRideResponse rideId responseId
    let previous = fromMaybe [] response.actionResults
        updated = [fromMaybe r (find (sameAction r) req.results) | r <- previous]
        added = [r | r <- req.results, not (any (sameAction r) previous)]
    QRFR.updateActionResults (Just $ updated <> added) response.id
  pure Success

-------------------------------------------------------------------------------
-- GET /internal/ride/{rideId}/feedback/response/{responseId}/retryableActions

-- | Actions of the answer that failed, or are still PENDING long after they started (their run was lost),
-- so a retry never repeats a ticket or report that went through or may still be going through. The
-- actions handed out are marked PENDING again under the ride's lock, so two retries do not both run
-- them. Actions whose rule was since removed from the question are not returned.
-- The question is read as this ride is served it (Config Pilot, pinned to the ride's transaction),
-- so a staged change's action rules apply to the rides it was rolled out to.
getRetryableActions :: Id DRide.Ride -> Id DRFR.RideFeedbackResponse -> Maybe Text -> Flow RetryableActionsRes
getRetryableActions rideId responseId apiKey = do
  (ride, booking) <- fetchAuthorisedRide rideId apiKey
  response <- fetchRideResponse rideId responseId
  answer <- case (response.status, response.answer) of
    (DRFR.ANSWERED, Just a) -> pure a
    _ -> throwError (InvalidRequest "Only answered responses have actions")
  config <- find ((== response.configId) . (.id)) <$> cityQuestions booking ride.merchantOperatingCityId Nothing >>= fromMaybeM (RideFeedbackConfigNotFound response.configId.getId)
  now <- getCurrentTime
  Hedis.withLockRedisAndReturnValue (submitLockKey rideId) submitLockSeconds $ do
    current <- fetchRideResponse rideId responseId
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
    pure RetryableActionsRes {responseId = current.id, questionKey = current.questionKey, answer, actions}

-------------------------------------------------------------------------------
-- Helpers

fetchRideResponse :: Id DRide.Ride -> Id DRFR.RideFeedbackResponse -> Flow DRFR.RideFeedbackResponse
fetchRideResponse rideId responseId = do
  response <- QRFR.findByPrimaryKey responseId >>= fromMaybeM (InvalidRequest $ "Ride feedback response " <> responseId.getId <> " not found")
  unless (response.rideId == rideId) $ throwError (InvalidRequest $ "Ride feedback response " <> responseId.getId <> " not found")
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

-- | The ride and its booking, after checking the caller holds the merchant's internal API key.
fetchAuthorisedRide :: Id DRide.Ride -> Maybe Text -> Flow (DRide.Ride, DB.Booking)
fetchAuthorisedRide rideId apiKey = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- CQM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  pure (ride, booking)

pickTranslation :: Language -> [Translation] -> Maybe Text
pickTranslation language translations =
  (.translation)
    <$> ( find ((== language) . (.language)) translations
            <|> find ((== ENGLISH) . (.language)) translations
            <|> listToMaybe translations
        )

-- | A response is accepted while the ride is in one of the question's stages. Once the ride has completed,
-- only a question already shown on this ride can still be answered: the rider app hears of the ride's end
-- a few seconds after this platform (via on_update), so a tap at drop-off can land just after completion.
submissionAllowed :: DRide.Ride -> SubmitState -> DRFC.RideFeedbackConfig -> Bool
submissionAllowed ride seenResponses config =
  statusAllowed ride.status config
    || (ride.status == DRide.COMPLETED && HM.member config.id seenResponses)

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

emptyQuestionsRes :: Id DRide.Ride -> UTCTime -> RideFeedbackQuestionsRes
emptyQuestionsRes rideId now = RideFeedbackQuestionsRes {rideId, serverTime = now, questions = [], followUpQuestions = []}

-- | Same rule and action type: one entry of a response's action results.
sameAction :: DRFR.RideFeedbackActionResult -> DRFR.RideFeedbackActionResult -> Bool
sameAction a b = a.ruleId == b.ruleId && a.actionType == b.actionType

-- | A PENDING action older than this is taken as lost (rider-app's own attempts finish well within it).
stalePendingSeconds :: NominalDiffTime
stalePendingSeconds = 300

submitLockKey :: Id DRide.Ride -> Text
submitLockKey rideId = "RideFeedback:SubmitLock:RideId-" <> rideId.getId

submitLockSeconds :: Hedis.ExpirationTime
submitLockSeconds = 10

maxResponsesPerRequest :: Int
maxResponsesPerRequest = 20

defaultMaxTextLength :: Int
defaultMaxTextLength = 1000

defaultMaxFiles :: Int
defaultMaxFiles = 3
