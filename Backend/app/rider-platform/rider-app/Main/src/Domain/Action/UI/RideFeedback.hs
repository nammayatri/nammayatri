module Domain.Action.UI.RideFeedback
  ( getRideFeedbackQuestions,
    postRideFeedback,
    getRideFeedback,
  )
where

import qualified API.Types.UI.RideFeedback as API
import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HM
import Data.List (nub, sortOn)
import qualified Data.Text as T
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideFeedbackConfig as DRFC
import qualified Domain.Types.RideFeedbackResponse as DRFR
import qualified Domain.Types.RideStatus as DRS
import qualified Domain.Types.RiderConfig as DRC
import Environment
import IssueManagement.Common (Translation (..))
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.RideFeedback.Actions (ActionEnv (..), triggerActions)
import SharedLogic.RideFeedback.Context
import SharedLogic.RideFeedback.Eligibility
import SharedLogic.RideFeedback.Events (clearEligibleQuestionsCache, eligibleQuestionsCacheKey)
import SharedLogic.RideFeedback.Selection (selectedLogicVersion)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.RideFeedbackConfig as CQRFC
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideFeedbackResponse as QRFR
import Tools.Error

-- | Result of the eligibility pass for one ride, cached briefly because the in-ride screen polls.
data EligibleQuestions = EligibleQuestions
  { rideStatus :: DRS.RideStatus,
    questions :: [EligibleQuestion],
    followUps :: [EligibleQuestion],
    nextPollAfterSeconds :: Maybe Int,
    -- | RIDE-FEEDBACK dynamic logic version that selected these questions.
    logicVersion :: Maybe Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data EligibleQuestion = EligibleQuestion
  { configId :: Id DRFC.RideFeedbackConfig,
    showAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-------------------------------------------------------------------------------
-- GET /ride/{rideId}/feedback/questions

getRideFeedbackQuestions ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DRide.Ride ->
  Maybe Language ->
  Flow API.RideFeedbackQuestionsRes
getRideFeedbackQuestions (mbPersonId, _) rideId mbLanguage = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  (ride, booking) <- fetchOwnedRide personId rideId
  now <- getCurrentTime
  configs <- CQRFC.findAllEnabledByMerchantOpCityId booking.merchantOperatingCityId
  -- Cheap exit for the common case: no question in this city applies to the ride's current status.
  if not (any (statusAllowed ride.status) configs)
    then pure $ emptyQuestionsRes rideId now
    else do
      eligible <-
        Hedis.safeGet (eligibleQuestionsCacheKey rideId) >>= \case
          Just cached | cached.rideStatus == ride.status -> pure cached
          _ -> do
            computed <- computeEligibleQuestions now personId ride booking configs
            Hedis.setExp (eligibleQuestionsCacheKey rideId) computed eligibleCacheTtl
            pure computed
      language <- resolveLanguage personId mbLanguage
      let configById = HM.fromList [(c.id, c) | c <- configs]
          render = mapMaybe (\q -> mkQuestionItem language q.showAt <$> HM.lookup q.configId configById)
      pure
        API.RideFeedbackQuestionsRes
          { rideId,
            serverTime = now,
            nextPollAfterSeconds = eligible.nextPollAfterSeconds,
            questions = render eligible.questions,
            followUpQuestions = render eligible.followUps
          }

computeEligibleQuestions :: UTCTime -> Id DP.Person -> DRide.Ride -> DB.Booking -> [DRFC.RideFeedbackConfig] -> Flow EligibleQuestions
computeEligibleQuestions now personId ride booking configs = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  evaluation <- evaluateRide now person ride booking configs
  let eligibleTop = [(c, t) | (c, Eligible t) <- evaluation.decisions]
  pure
    EligibleQuestions
      { rideStatus = ride.status,
        questions = map (\(c, t) -> EligibleQuestion c.id t) (sortOn (priorityKey . fst) eligibleTop),
        followUps = map (\c -> EligibleQuestion c.id now) evaluation.followUps,
        nextPollAfterSeconds = if any (\case (_, Pending _) -> True; _ -> False) evaluation.decisions then Just pendingPollSeconds else Nothing,
        logicVersion = evaluation.logicVersion
      }

mkQuestionItem :: Language -> UTCTime -> DRFC.RideFeedbackConfig -> API.FeedbackQuestionItem
mkQuestionItem language showAt config =
  API.FeedbackQuestionItem
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
      API.FeedbackOptionItem
        { key = o.key,
          label = fromMaybe o.key (pickTranslation language o.label),
          iconUrl = o.iconUrl,
          nextQuestionKey = o.nextQuestionKey,
          requiresText = fromMaybe False o.requiresText,
          isExclusive = fromMaybe False o.isExclusive
        }
    mkInputConfig ic =
      API.FeedbackInputConfigRes
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
-- POST /ride/{rideId}/feedback

postRideFeedback ::
  (Maybe (Id DP.Person), Id DM.Merchant) ->
  Id DRide.Ride ->
  Maybe Language ->
  API.SubmitRideFeedbackReq ->
  Flow API.SubmitRideFeedbackRes
postRideFeedback (mbPersonId, _) rideId mbLanguage req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  when (length req.responses > maxResponsesPerRequest) $
    throwError $ InvalidRequest ("At most " <> show maxResponsesPerRequest <> " responses per request")
  (ride, booking) <- fetchOwnedRide personId rideId
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  configs <- CQRFC.findAllEnabledByMerchantOpCityId booking.merchantOperatingCityId
  now <- getCurrentTime
  riderConfig <- fetchRiderConfig booking
  let language = fromMaybe ENGLISH (mbLanguage <|> person.language)
      configById = HM.fromList [(c.id, c) | c <- configs]
      localTime = addUTCTime (fromIntegral riderConfig.timeDiffFromUtc.getSeconds) now
  -- Same version the questions call used for this rider (stable toss), saved for A/B analysis.
  logicVersion <- selectedLogicVersion booking.merchantOperatingCityId personId localTime
  -- Only needed when an answer can fire actions; fetched once per request.
  mbActionDeps <-
    if any (\i -> i.status == DRFR.ANSWERED && maybe False (not . null . fromMaybe [] . (.actionRules)) (HM.lookup i.questionId configById)) req.responses
      then do
        merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
        city <- CQMOC.findById booking.merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
        pure $ Just (merchant, riderConfig, city)
      else pure Nothing
  results <-
    Hedis.withLockRedisAndReturnValue (submitLockKey rideId) submitLockSeconds $ do
      existing <- QRFR.findAllByRideId ride.id
      let initial = HM.fromList [(r.configId, r) | r <- existing]
          env = SubmitEnv {..}
      (acc, _) <- foldM (\(results', seenResponses) item -> first (: results') <$> submitItem env seenResponses item) ([], initial) req.responses
      pure $ reverse acc
  clearEligibleQuestionsCache rideId
  pure API.SubmitRideFeedbackRes {results}

data SubmitEnv = SubmitEnv
  { now :: UTCTime,
    language :: Language,
    person :: DP.Person,
    ride :: DRide.Ride,
    booking :: DB.Booking,
    configById :: HM.HashMap (Id DRFC.RideFeedbackConfig) DRFC.RideFeedbackConfig,
    existing :: [DRFR.RideFeedbackResponse],
    logicVersion :: Maybe Int,
    mbActionDeps :: Maybe (DM.Merchant, DRC.RiderConfig, Context.City)
  }

type SubmitState = HM.HashMap (Id DRFC.RideFeedbackConfig) DRFR.RideFeedbackResponse

submitItem :: SubmitEnv -> SubmitState -> API.FeedbackResponseItem -> Flow (API.FeedbackSubmitResult, SubmitState)
submitItem env seenResponses item =
  case HM.lookup item.questionId env.configById of
    Nothing -> rejected "QUESTION_NOT_FOUND"
    Just config
      | not (submissionAllowed env.now env.ride config) -> rejected "RIDE_STATUS_NOT_ALLOWED"
      | Just prev <- HM.lookup config.id seenResponses, isFinal prev.status -> rejected "ALREADY_ANSWERED"
      | item.status == DRFR.ANSWERED -> case validateAnswer config item.answer of
        Left code -> rejected code
        Right answer -> do
          saved <- upsertResponse env seenResponses config item (Just answer)
          whenJust env.mbActionDeps $ \(merchant, riderConfig, city) -> do
            let ctx = buildRideFeedbackContext (contextInput city riderConfig)
            triggerActions
              ActionEnv {merchant, riderConfig, person = env.person, ride = env.ride, booking = env.booking, config, response = saved, answer}
              (withAnswer answer ctx)
          accepted config saved (config.acknowledgement >>= pickTranslation env.language)
      | otherwise -> do
        saved <- upsertResponse env seenResponses config item Nothing
        accepted config saved Nothing
  where
    rejected code = pure (API.FeedbackSubmitResult {questionId = item.questionId, accepted = False, errorCode = Just code, acknowledgement = Nothing}, seenResponses)
    accepted config saved acknowledgement = pure (API.FeedbackSubmitResult {questionId = item.questionId, accepted = True, errorCode = Nothing, acknowledgement}, HM.insert config.id saved seenResponses)
    contextInput city riderConfig =
      ContextInput
        { now = env.now,
          timeDiffFromUtc = riderConfig.timeDiffFromUtc,
          booking = env.booking,
          ride = env.ride,
          person = env.person,
          city,
          rideResponses = HM.elems seenResponses,
          historyResponses = [],
          rideEvents = []
        }

upsertResponse :: SubmitEnv -> SubmitState -> DRFC.RideFeedbackConfig -> API.FeedbackResponseItem -> Maybe DRFR.RideFeedbackAnswer -> Flow DRFR.RideFeedbackResponse
upsertResponse env seenResponses config item answer = do
  let parentResponseId = item.parentQuestionId >>= (`HM.lookup` seenResponses) <&> (.id)
      secondsIntoRide = (\t -> floor (diffUTCTime env.now t)) <$> env.ride.rideStartTime
      isShown = item.status == DRFR.SHOWN
  case HM.lookup config.id seenResponses of
    Just prev -> do
      let updated =
            (prev :: DRFR.RideFeedbackResponse)
              { DRFR.status = item.status,
                DRFR.answer = answer <|> prev.answer,
                DRFR.selectedOptionKeys = (answer >>= (.selectedOptionKeys)) <|> prev.selectedOptionKeys,
                DRFR.shownCount = if isShown then prev.shownCount + 1 else prev.shownCount,
                DRFR.configVersion = config.version,
                DRFR.logicVersion = prev.logicVersion <|> env.logicVersion,
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
                personId = env.person.id,
                merchantId = env.booking.merchantId,
                merchantOperatingCityId = env.booking.merchantOperatingCityId,
                configId = config.id,
                questionKey = config.questionKey,
                configVersion = config.version,
                logicVersion = env.logicVersion,
                parentResponseId,
                status = item.status,
                answer,
                selectedOptionKeys = answer >>= (.selectedOptionKeys),
                shownCount = 1,
                rideStatusAtResponse = Just env.ride.status,
                secondsIntoRide,
                lat = item.location <&> (.lat),
                lon = item.location <&> (.lon),
                vehicleServiceTierType = Just env.booking.vehicleServiceTierType,
                vehicleVariant = Just (show env.ride.vehicleVariant),
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
-- GET /ride/{rideId}/feedback

getRideFeedback :: (Maybe (Id DP.Person), Id DM.Merchant) -> Id DRide.Ride -> Flow API.RideFeedbackSubmittedRes
getRideFeedback (mbPersonId, _) rideId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  void $ fetchOwnedRide personId rideId
  responses <- QRFR.findAllByRideId rideId
  pure
    API.RideFeedbackSubmittedRes
      { responses =
          [ API.SubmittedFeedbackItem
              { questionId = r.configId,
                questionKey = r.questionKey,
                status = r.status,
                answer = r.answer,
                updatedAt = r.updatedAt
              }
            | r <- responses
          ]
      }

-------------------------------------------------------------------------------
-- Helpers

fetchOwnedRide :: Id DP.Person -> Id DRide.Ride -> Flow (DRide.Ride, DB.Booking)
fetchOwnedRide personId rideId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  unless (booking.riderId == personId) $ throwError (RideFeedbackAccessDenied rideId.getId)
  pure (ride, booking)

resolveLanguage :: Id DP.Person -> Maybe Language -> Flow Language
resolveLanguage _ (Just language) = pure language
resolveLanguage personId Nothing = fromMaybe ENGLISH . (>>= (.language)) <$> QPerson.findById personId

pickTranslation :: Language -> [Translation] -> Maybe Text
pickTranslation language translations =
  (.translation)
    <$> ( find ((== language) . (.language)) translations
            <|> find ((== ENGLISH) . (.language)) translations
            <|> listToMaybe translations
        )

-- | Answers may still arrive shortly after the ride ends (the sheet was open when it completed).
submissionAllowed :: UTCTime -> DRide.Ride -> DRFC.RideFeedbackConfig -> Bool
submissionAllowed now ride config =
  statusAllowed ride.status config
    || (ride.status == DRS.COMPLETED && maybe False (\endedAt -> diffUTCTime now endedAt <= submitGraceAfterRideEnd) ride.rideEndTime)

emptyQuestionsRes :: Id DRide.Ride -> UTCTime -> API.RideFeedbackQuestionsRes
emptyQuestionsRes rideId now = API.RideFeedbackQuestionsRes {rideId, serverTime = now, nextPollAfterSeconds = Nothing, questions = [], followUpQuestions = []}

submitLockKey :: Id DRide.Ride -> Text
submitLockKey rideId = "RideFeedback:SubmitLock:RideId-" <> rideId.getId

eligibleCacheTtl :: Hedis.ExpirationTime
eligibleCacheTtl = 30

submitLockSeconds :: Hedis.ExpirationTime
submitLockSeconds = 10

pendingPollSeconds :: Int
pendingPollSeconds = 60

submitGraceAfterRideEnd :: NominalDiffTime
submitGraceAfterRideEnd = 600

maxResponsesPerRequest :: Int
maxResponsesPerRequest = 20

defaultMaxTextLength :: Int
defaultMaxTextLength = 1000

defaultMaxFiles :: Int
defaultMaxFiles = 3
