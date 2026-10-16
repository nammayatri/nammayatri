-- | Feedback anywhere in the app (home, search, tickets, journeys). The app fetches the manifest once
-- per session; when the rider reaches a placement with a question, it reports SHOWN first and renders
-- the card only if that is accepted (the cap and the subject are checked then), then reports what the
-- rider did. Answers are stored here (app_feedback_response) and their actions run here.
module Domain.Action.UI.AppFeedback
  ( getAppFeedbackManifest,
    postAppFeedback,
    runActionsForResponse,
  )
where

import qualified API.Types.UI.AppFeedback as API
import qualified API.Types.UI.RideFeedback as RideAPI
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (sortOn)
import qualified Domain.Types.AppFeedbackConfig as DAFC
import qualified Domain.Types.AppFeedbackResponse as DAFR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Feedback.Question (answerScore, pendingResult, priorityKey, validateAnswer)
import qualified Lib.Feedback.Question as LFQ
import qualified Lib.Feedback.Types as LFT
import Lib.Feedback.View (FeedbackQuestionItem, QuestionTiming (..), mkQuestionItem)
import qualified SharedLogic.AppFeedback.Cap as Cap
import SharedLogic.AppFeedback.Context
import SharedLogic.AppFeedback.Eligibility
import SharedLogic.AppFeedback.Placement (placementSubject)
import qualified SharedLogic.RideFeedback.Actions as Actions
import qualified SharedLogic.RideFeedback.Types as RFT
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.AppFeedbackResponse as QAFR
import Tools.Error

-------------------------------------------------------------------------------
-- GET /appFeedback/manifest

getAppFeedbackManifest :: (Maybe (Id DP.Person), Id DM.Merchant) -> Maybe Language -> Flow API.AppFeedbackManifestRes
getAppFeedbackManifest (mbPersonId, _) mbLanguage = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  st <- loadRiderState personId
  configs <- cityQuestions st.city.id (Just True)
  let language = fromMaybe ENGLISH (mbLanguage <|> st.person.language)
      emptyManifest = API.AppFeedbackManifestRes {serverTime = st.now, nextAllowedAt = st.nextAllowedAt, questions = [], followUpQuestions = []}
  -- Cheap exit for the common case: no question in this city has a placement.
  if all (null . fromMaybe [] . (.placements)) configs
    then pure emptyManifest
    else do
      evaluation <- evaluateRider st configs Nothing Nothing
      let eligible = sortOn priorityKey [c | (c, Eligible) <- evaluation.decisions]
          item c =
            API.AppFeedbackManifestItem
              { placements = riderPlacements st c,
                showAfterSeconds = c.displayTrigger >>= (.showAfterSeconds),
                checksSubject = isJust c.subjectCondition,
                bypassCap = fromMaybe False c.bypassCap,
                question = questionItem language st.now c
              }
      pure
        emptyManifest
          { API.questions = map item eligible,
            API.followUpQuestions = map (questionItem language st.now) evaluation.followUps
          }

questionItem :: Language -> UTCTime -> DAFC.AppFeedbackConfig -> FeedbackQuestionItem
questionItem language now config =
  mkQuestionItem config.id.getId language QuestionTiming {showAt = now, autoDismissAfterSeconds = config.displayTrigger >>= (.autoDismissAfterSeconds)} config

-------------------------------------------------------------------------------
-- POST /appFeedback

postAppFeedback :: (Maybe (Id DP.Person), Id DM.Merchant) -> API.AppFeedbackSubmitReq -> Flow API.AppFeedbackSubmitRes
postAppFeedback (mbPersonId, _) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  when (length req.responses > maxResponsesPerRequest) $
    throwError $ InvalidRequest ("At most " <> show maxResponsesPerRequest <> " responses per request")
  st0 <- loadRiderState personId
  subject <- loadSubject st0.now st0.person (placementSubject req.placement) req.subjectId
  configs <- cityQuestions st0.city.id (Just True)
  configPilotVersions <- servedConfigVersions st0.city.id
  evaluation <- evaluateRider st0 configs (Just req.placement) subject.ctx
  let eligibleTop = HS.fromList [c.id | (c, Eligible) <- evaluation.decisions]
      configById = HM.fromList [(c.id.getId, c) | c <- configs]
  (results, actionRuns) <-
    Hedis.withLockRedisAndReturnValue (Cap.capLockKey personId) lockSeconds $ do
      -- Re-read under the lock: another request may have just shown a card or answered.
      st <- loadRiderState personId
      let env = SubmitEnv {st, subject, placement = req.placement, configById, eligibleTop, context = evaluation.context, logicVersion = evaluation.logicVersion, configPilotVersions}
          initial = HM.fromList [(c.id, r) | c <- configs, Just r <- [listToMaybe (subjectResponses st c subject.subjectId)]]
      (acc, _) <- foldM (\(done, seen) item -> first (: done) <$> submitItem env seen item) ([], initial) req.responses
      pure (reverse (map fst acc), mapMaybe snd acc)
  forM_ actionRuns (runActionsForResponse st0.person)
  now <- getCurrentTime
  shownTimes <- Cap.recentShownTimes personId now
  pure API.AppFeedbackSubmitRes {results, nextAllowedAt = Cap.nextAllowedAt (Cap.capSettings st0.riderConfig) now shownTimes}

data SubmitEnv = SubmitEnv
  { st :: RiderState,
    subject :: Subject,
    placement :: DAFC.AppFeedbackPlacement,
    configById :: HM.HashMap Text DAFC.AppFeedbackConfig,
    -- | Top-level questions the APP-FEEDBACK logic selected at this placement.
    eligibleTop :: HS.HashSet (Id DAFC.AppFeedbackConfig),
    context :: AppFeedbackContext,
    logicVersion :: Maybe Int,
    configPilotVersions :: [Int]
  }

-- | The responses on this subject so far, by question (stored, or earlier in the same request).
type SubmitState = HM.HashMap (Id DAFC.AppFeedbackConfig) DAFR.AppFeedbackResponse

-- | An answer whose action rules matched, to run once the lock is released.
type ActionRun = (DAFR.AppFeedbackResponse, DAFC.AppFeedbackConfig, [LFT.MatchedFeedbackAction])

submitItem :: SubmitEnv -> SubmitState -> API.AppFeedbackResponseItem -> Flow ((API.AppFeedbackSubmitResult, Maybe ActionRun), SubmitState)
submitItem env seen item =
  case HM.lookup item.questionId env.configById of
    Nothing -> rejected "QUESTION_NOT_FOUND"
    Just config -> case HM.lookup config.id seen of
      Just prev | isFinal prev.status -> rejected "ALREADY_ANSWERED"
      mbPrev -> do
        let reached = followUpReached env seen config
            check = case mbPrev of
              Just _ -> Right ()
              Nothing -> decideAtPlacement env.st env.eligibleTop env.context item.status reached config
        case check of
          Left code -> rejected code
          Right ()
            | item.status == LFT.ANSWERED -> case validateAnswer config item.answer of
              Left code -> rejected code
              Right answer -> do
                saved <- upsertResponse env mbPrev config item.status (Just answer)
                let (actions, errors) = LFQ.matchActionRules config (withAnswer answer env.context)
                forM_ errors $ \err -> logError $ "AppFeedback: skipping " <> err
                saved' <-
                  if null actions
                    then pure saved
                    else do
                      let pending = map (pendingResult env.st.now) actions
                      QAFR.updateActionResults (Just pending) saved.id
                      pure saved {DAFR.actionResults = Just pending}
                countCard mbPrev reached saved'
                accepted config saved' (if null actions then Nothing else Just (saved', config, actions))
            | otherwise -> do
              saved <- upsertResponse env mbPrev config item.status Nothing
              countCard mbPrev reached saved
              accepted config saved Nothing
  where
    rejected code = pure ((API.AppFeedbackSubmitResult {questionId = item.questionId, accepted = False, errorCode = Just code}, Nothing), seen)
    accepted config saved run = pure ((API.AppFeedbackSubmitResult {questionId = item.questionId, accepted = True, errorCode = Nothing}, run), HM.insert config.id saved seen)
    -- A new card (not a follow-up inside one) takes a slot of the rider's cap.
    countCard mbPrev reached saved = when (isNothing mbPrev && not reached) $ Cap.recordShown env.st.person.id env.st.now saved.id.getId

-- | Whether an answer already given on this subject (stored, or earlier in the same request) picked an
-- option that opens this question.
followUpReached :: SubmitEnv -> SubmitState -> DAFC.AppFeedbackConfig -> Bool
followUpReached env seen config =
  or
    [ True
      | r <- HM.elems seen,
        r.status == LFT.ANSWERED,
        Just parent <- [HM.lookup r.configId.getId env.configById],
        o <- fromMaybe [] parent.options,
        o.nextQuestionKey == Just config.questionKey,
        o.key `elem` fromMaybe [] (r.answer >>= (.selectedOptionKeys))
    ]

upsertResponse :: SubmitEnv -> Maybe DAFR.AppFeedbackResponse -> DAFC.AppFeedbackConfig -> LFT.FeedbackResponseStatus -> Maybe LFT.FeedbackAnswer -> Flow DAFR.AppFeedbackResponse
upsertResponse env mbPrev config status answer = do
  let now = env.st.now
      nonEmptyVersions = if null env.configPilotVersions then Nothing else Just env.configPilotVersions
  case mbPrev of
    Just prev -> do
      let updated =
            prev
              { DAFR.status = status,
                DAFR.answer = answer <|> prev.answer,
                DAFR.answerScore = maybe prev.answerScore (answerScore config) answer,
                DAFR.logicVersion = prev.logicVersion <|> env.logicVersion,
                DAFR.configPilotVersions = prev.configPilotVersions <|> nonEmptyVersions,
                DAFR.updatedAt = now
              }
      QAFR.updateByPrimaryKey updated
      pure updated
    Nothing -> do
      responseId <- generateGUID
      let created =
            DAFR.AppFeedbackResponse
              { id = responseId,
                personId = env.st.person.id,
                merchantId = env.st.person.merchantId,
                merchantOperatingCityId = env.st.city.id,
                configId = config.id,
                questionKey = config.questionKey,
                questionType = config.questionType,
                placement = env.placement,
                subjectType = env.subject.subjectType,
                subjectId = env.subject.subjectId,
                logicVersion = env.logicVersion,
                configPilotVersions = nonEmptyVersions,
                status,
                answer,
                answerScore = answerScore config =<< answer,
                actionResults = Nothing,
                createdAt = now,
                updatedAt = now
              }
      QAFR.create created
      pure created

-- | Runs an answer's matched actions in the background and stores their results on the response.
runActionsForResponse :: DP.Person -> ActionRun -> Flow ()
runActionsForResponse person (response, config, actions) = do
  merchant <- CQM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  st <- loadRiderState person.id
  answer <- response.answer & fromMaybeM (InvalidRequest "Only answered responses have actions")
  riderActions <- mapM toRiderAction actions
  let env =
        Actions.ActionEnv
          { merchant,
            riderConfig = st.riderConfig,
            person,
            ride = Nothing,
            booking = Nothing,
            subjectLines =
              [ "App feedback : " <> show response.placement,
                "Subject : " <> show response.subjectType <> " " <> response.subjectId
              ],
            questionKey = config.questionKey,
            answer = toRideAnswer answer
          }
  Actions.runActionsInBackground env riderActions $ \results -> do
    libResults <- mapM fromRiderResult results
    -- Each replaces the stored result of the same action; attempts add up so the inspector shows every try.
    current <- QAFR.findByPrimaryKey response.id
    let previous = fromMaybe [] (current >>= (.actionResults))
        updated = [maybe r (\new -> new {LFT.attempts = r.attempts + new.attempts}) (find (LFQ.sameAction r) libResults) | r <- previous]
        added = [r | r <- libResults, not (any (LFQ.sameAction r) previous)]
    QAFR.updateActionResults (Just $ updated <> added) response.id
  where
    toRideAnswer (a :: LFT.FeedbackAnswer) =
      RideAPI.RideFeedbackAnswer {selectedOptionKeys = a.selectedOptionKeys, text = a.text, number = a.number, rating = a.rating, mediaFileIds = a.mediaFileIds}
    -- The runner's action types are the wire types of ride feedback: same names.
    toRiderAction :: LFT.MatchedFeedbackAction -> Flow RFT.MatchedFeedbackAction
    toRiderAction = viaJson
    fromRiderResult :: RFT.RideFeedbackActionResult -> Flow LFT.FeedbackActionResult
    fromRiderResult = viaJson
    viaJson :: (ToJSON a, FromJSON b) => a -> Flow b
    viaJson a = case A.fromJSON (A.toJSON a) of
      A.Success b -> pure b
      A.Error err -> throwError $ InternalError ("AppFeedback: action type mismatch: " <> show err)

lockSeconds :: Hedis.ExpirationTime
lockSeconds = 10

maxResponsesPerRequest :: Int
maxResponsesPerRequest = 20
