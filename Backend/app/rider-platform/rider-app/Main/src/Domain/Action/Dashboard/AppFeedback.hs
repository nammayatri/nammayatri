-- | Dashboard APIs of app feedback in one merchant + city: the placements, what a rider would be asked at
-- a placement and why (the inspector), the rider's answers, and retrying an answer's failed actions.
-- The questions themselves are managed through the generic Config Pilot APIs (AppFeedbackConfig), and
-- who gets them through the APP-FEEDBACK logic (NammaTag APIs).
module Domain.Action.Dashboard.AppFeedback
  ( getAppFeedbackMeta,
    getAppFeedbackInspectPreview,
    getAppFeedbackInspectResponses,
    postAppFeedbackResponseRetryActions,
  )
where

import qualified API.Types.RiderPlatform.Management.AppFeedback as Common
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (sortOn)
import qualified Domain.Action.UI.AppFeedback as UIAppFeedback
import qualified Domain.Types.AppFeedbackConfig as DAFC
import qualified Domain.Types.AppFeedbackResponse as DAFR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Feedback.Question (collectFollowUps, priorityKey)
import Lib.Feedback.Rule (supportedOperatorNames)
import qualified Lib.Feedback.Types as LFT
import Lib.Feedback.View (uiLayouts)
import qualified SharedLogic.AppFeedback.Cap as Cap
import SharedLogic.AppFeedback.Context (loadSubject)
import SharedLogic.AppFeedback.Eligibility
import SharedLogic.AppFeedback.Placement (placementInfo, placementSubject)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.AppFeedbackResponse as QAFR
import qualified Storage.Queries.Person as QPerson
import Tools.Error

-------------------------------------------------------------------------------
-- GET /appFeedback/meta

getAppFeedbackMeta :: ShortId DM.Merchant -> Context.City -> Flow Common.AppFeedbackMetaRes
getAppFeedbackMeta merchantShortId opCity = do
  void $ resolveCity merchantShortId opCity
  pure
    Common.AppFeedbackMetaRes
      { placements = [Common.AppFeedbackPlacementInfo {placement, subjectType} | (placement, subjectType) <- placementInfo],
        uiLayouts,
        supportedOperators = supportedOperatorNames
      }

-------------------------------------------------------------------------------
-- GET /appFeedback/inspect/{customerId}/preview

-- | What the rider would be asked at the placement now: the APP-FEEDBACK logic on the rider at the
-- placement, then each question's own limits as a SHOWN would be checked. Nothing is written.
getAppFeedbackInspectPreview :: ShortId DM.Merchant -> Context.City -> Text -> Maybe Text -> DAFC.AppFeedbackPlacement -> Flow Common.AppFeedbackPreviewRes
getAppFeedbackInspectPreview merchantShortId opCity customerId mbSubjectId placement = do
  (_, city) <- resolveCity merchantShortId opCity
  person <- fetchCityPerson city customerId
  st <- loadRiderState person.id
  subject <- loadSubject st.now st.person (placementSubject placement) mbSubjectId
  configs <- cityQuestions city.id (Just True)
  evaluation <- evaluateRider st configs (Just placement) subject.ctx
  let eligibleTop = HS.fromList [c.id | (c, Eligible) <- evaluation.decisions]
      started c = listToMaybe (subjectResponses st c subject.subjectId)
      reasonOf isFollowUp c decision = case (decision, started c) of
        (Excluded r, _) -> Just r
        (Eligible, Just r) | isFinal r.status -> Just "ALREADY_ANSWERED"
        (Eligible, Just _) -> Nothing -- shown, not finished: it can still be answered
        (Eligible, Nothing)
          | isFollowUp -> Nothing
          | otherwise -> either Just (const Nothing) (decideAtPlacement st eligibleTop evaluation.context LFT.SHOWN False c)
      tops = [(c, reasonOf False c d) | (c, d) <- sortOn (priorityKey . fst) evaluation.decisions]
      -- A follow-up is shown here only through a question that can be asked here.
      reachable = HS.fromList . map (.id) $ collectFollowUps (HM.fromList [(c.questionKey, c) | c <- configs]) (const True) [c | (c, Nothing) <- tops]
      followUpReason c
        | HS.member c.id reachable = reasonOf True c Eligible
        | otherwise = Just "FOLLOW_UP_NOT_REACHED"
      evaluation' isFollowUp c reason = Common.AppFeedbackEvaluation {configId = c.id.getId, questionKey = c.questionKey, eligible = isNothing reason, isFollowUp, reason}
  pure
    Common.AppFeedbackPreviewRes
      { placement,
        subjectType = subject.subjectType,
        subjectId = subject.subjectId,
        logicVersion = evaluation.logicVersion,
        selectedQuestionKeys = evaluation.selectedKeys,
        nextAllowedAt = st.nextAllowedAt,
        context = A.toJSON evaluation.context,
        evaluations =
          [evaluation' False c reason | (c, reason) <- tops]
            <> [evaluation' True c (followUpReason c) | c <- evaluation.followUps]
      }

-------------------------------------------------------------------------------
-- GET /appFeedback/inspect/{customerId}/responses

getAppFeedbackInspectResponses :: ShortId DM.Merchant -> Context.City -> Text -> Flow [Common.AppFeedbackResponseRes]
getAppFeedbackInspectResponses merchantShortId opCity customerId = do
  (_, city) <- resolveCity merchantShortId opCity
  person <- fetchCityPerson city customerId
  responses <- QAFR.findLatestByPersonId (Just 200) Nothing person.id
  pure
    [ Common.AppFeedbackResponseRes
        { id = r.id.getId,
          configId = r.configId.getId,
          questionKey = r.questionKey,
          questionType = r.questionType,
          placement = r.placement,
          subjectType = r.subjectType,
          subjectId = r.subjectId,
          logicVersion = r.logicVersion,
          configPilotVersions = r.configPilotVersions,
          status = r.status,
          answer = r.answer,
          answerScore = r.answerScore,
          actionResults = r.actionResults,
          createdAt = r.createdAt,
          updatedAt = r.updatedAt
        }
      | r <- responses,
        r.merchantOperatingCityId == city.id
    ]

-------------------------------------------------------------------------------
-- POST /appFeedback/response/{responseId}/retryActions

-- | Re-runs the answer's actions that failed, or are still PENDING long after they started (their run was
-- lost), so a retry never repeats a ticket that went through or may still be going through. They are
-- marked PENDING again under the rider's lock, so two retries do not both run them. Actions whose rule
-- was since removed from the question are not run.
postAppFeedbackResponseRetryActions :: ShortId DM.Merchant -> Context.City -> Text -> Flow APISuccess
postAppFeedbackResponseRetryActions merchantShortId opCity responseId = do
  (_, city) <- resolveCity merchantShortId opCity
  response <- QAFR.findByPrimaryKey (Id responseId) >>= fromMaybeM (InvalidRequest $ "App feedback response " <> responseId <> " not found")
  unless (response.merchantOperatingCityId == city.id) $ throwError (InvalidRequest $ "App feedback response " <> responseId <> " not found")
  unless (response.status == LFT.ANSWERED) $ throwError (InvalidRequest "Only answered responses have actions")
  configs <- cityQuestions city.id Nothing
  config <- find ((== response.configId) . (.id)) configs & fromMaybeM (AppFeedbackConfigNotFound response.configId.getId)
  person <- QPerson.findById response.personId >>= fromMaybeM (PersonNotFound response.personId.getId)
  now <- getCurrentTime
  (claimedResponse, actions) <-
    Hedis.withLockRedisAndReturnValue (Cap.capLockKey person.id) 10 $ do
      current <- QAFR.findByPrimaryKey response.id >>= fromMaybeM (InvalidRequest $ "App feedback response " <> responseId <> " not found")
      let results = fromMaybe [] current.actionResults
          retryable r = r.status == LFT.FAILED || (r.status == LFT.PENDING && diffUTCTime now r.updatedAt > stalePendingSeconds)
          due = filter retryable results
          actions =
            [ LFT.MatchedFeedbackAction {ruleId = rule.ruleId, actionType = action.actionType, params = action.params}
              | rule <- fromMaybe [] config.actionRules,
                action <- rule.actions,
                any (\r -> r.ruleId == rule.ruleId && r.actionType == action.actionType) due
            ]
          claimed = [if any (\a -> a.ruleId == r.ruleId && a.actionType == r.actionType) actions then r {LFT.status = LFT.PENDING, LFT.updatedAt = now} else r | r <- results]
      unless (null actions) $ QAFR.updateActionResults (Just claimed) current.id
      pure (current {DAFR.actionResults = Just claimed}, actions)
  when (null actions) $ throwError (InvalidRequest "No failed or pending actions to retry")
  UIAppFeedback.runActionsForResponse person (claimedResponse, config, actions)
  pure Success

-------------------------------------------------------------------------------
-- Helpers

resolveCity :: ShortId DM.Merchant -> Context.City -> Flow (DM.Merchant, DMOC.MerchantOperatingCity)
resolveCity merchantShortId opCity = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  city <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  pure (merchant, city)

-- | The rider, who must belong to the city the dashboard is looking at.
fetchCityPerson :: DMOC.MerchantOperatingCity -> Text -> Flow DP.Person
fetchCityPerson city customerId = do
  person <- QPerson.findById (Id customerId) >>= fromMaybeM (PersonNotFound customerId)
  unless (person.merchantOperatingCityId == city.id) $ throwError (PersonNotFound customerId)
  pure person

-- | A PENDING action older than this is taken as lost (a run finishes well within it).
stalePendingSeconds :: NominalDiffTime
stalePendingSeconds = 300
