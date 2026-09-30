module SharedLogic.RideFeedback.Actions
  ( ActionEnv (..),
    triggerActions,
    retryFailedActions,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Domain.Action.UI.Support as DSupport
import qualified Domain.Types.Booking as DB
import Domain.Types.EmptyDynamicParam (EmptyDynamicParam (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideFeedbackConfig as DRFC
import qualified Domain.Types.RideFeedbackResponse as DRFR
import qualified Domain.Types.RiderConfig as DRC
import Environment
import qualified EulerHS.Language as L
import qualified IssueManagement.Common as IC
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.Person as SLP
import SharedLogic.RideFeedback.Context (RideFeedbackContext)
import SharedLogic.RideFeedback.Events (clearEligibleQuestionsCache)
import SharedLogic.RideFeedback.Rule (evaluateRule)
import qualified SharedLogic.Scheduler.Jobs.SafetyCSAlert as SIVR
import qualified Slack.Flow as Slack
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideFeedbackResponse as QRFR
import qualified Tools.Notifications as Notify
import qualified Tools.Ticket as TTicket

data ActionEnv = ActionEnv
  { merchant :: DM.Merchant,
    riderConfig :: DRC.RiderConfig,
    person :: DP.Person,
    ride :: DRide.Ride,
    booking :: DB.Booking,
    config :: DRFC.RideFeedbackConfig,
    response :: DRFR.RideFeedbackResponse,
    answer :: DRFR.RideFeedbackAnswer
  }

-- | Evaluates the question's action rules against the answer, records every matched action as
-- PENDING (so a lost pod still leaves an audit trail), then runs them in the background.
triggerActions :: ActionEnv -> RideFeedbackContext -> Flow ()
triggerActions env ctx = do
  matched <- concat <$> mapM matchRule (fromMaybe [] env.config.actionRules)
  unless (null matched) $ do
    now <- getCurrentTime
    QRFR.updateActionResults (Just $ map (mkResult now DRFR.PENDING 0 Nothing Nothing) matched) env.response.id
    fork "RideFeedback:actions" $ do
      results <- mapM (runAction env) matched
      QRFR.updateActionResults (Just results) env.response.id
  where
    matchRule rule = case evaluateRule rule.condition ctx of
      Right True -> pure $ map (rule.ruleId,) rule.actions
      Right False -> pure []
      Left err -> do
        logError $ "RideFeedback: skipping action rule " <> rule.ruleId <> " of " <> env.config.questionKey <> ": " <> err
        pure []

-- | Re-runs, in the background, only the actions whose last result is not SUCCESS (so a retry never
-- repeats a ticket or report that already went through). Returns how many actions were scheduled.
-- Actions whose rule was since removed from the question cannot be retried and keep their old result.
retryFailedActions :: ActionEnv -> Flow Int
retryFailedActions env = do
  let previous = fromMaybe [] env.response.actionResults
      isRetryable r = r.status /= DRFR.SUCCESS
      toRetry =
        [ (rule.ruleId, action)
          | rule <- fromMaybe [] env.config.actionRules,
            action <- rule.actions,
            any (\r -> isRetryable r && r.ruleId == rule.ruleId && r.actionType == action.actionType) previous
        ]
  unless (null toRetry) $
    fork "RideFeedback:retryActions" $ do
      retried <- mapM (runAction env) toRetry
      let merged = [fromMaybe r (find (\n -> n.ruleId == r.ruleId && n.actionType == r.actionType) retried) | r <- previous]
      QRFR.updateActionResults (Just merged) env.response.id
  pure (length toRetry)

mkResult :: UTCTime -> DRFR.RideFeedbackActionStatus -> Int -> Maybe Text -> Maybe Text -> (Text, DRFC.FeedbackAction) -> DRFR.RideFeedbackActionResult
mkResult now status attempts externalRef errorMessage (ruleId, action) =
  DRFR.RideFeedbackActionResult
    { ruleId,
      actionType = action.actionType,
      status,
      attempts,
      externalRef,
      errorMessage,
      updatedAt = now
    }

maxAttempts :: Int
maxAttempts = 3

-- | Config errors (bad params) fail at once; runtime exceptions are retried with a growing delay.
runAction :: ActionEnv -> (Text, DRFC.FeedbackAction) -> Flow DRFR.RideFeedbackActionResult
runAction env matched@(_, action) = go 1
  where
    go attempt = do
      res <- withTryCatch "RideFeedback:runAction" (executeAction env action)
      now <- getCurrentTime
      case res of
        Right (Right externalRef) -> pure $ mkResult now DRFR.SUCCESS attempt externalRef Nothing matched
        Right (Left configErr) -> pure $ mkResult now DRFR.FAILED attempt Nothing (Just configErr) matched
        Left err
          | attempt < maxAttempts -> threadDelaySec (Seconds attempt) >> go (attempt + 1)
          | otherwise -> do
            let summary = summarizeError err
            logError $ "RideFeedback: action " <> show action.actionType <> " failed for response " <> env.response.id.getId <> ": " <> summary
            pure $ mkResult now DRFR.FAILED attempt Nothing (Just summary) matched

-- | Short, secret-free description of a failure for storage and logs. HTTP client errors render
-- the whole request (including auth headers such as the BPP internal token) after "Request {",
-- so the text is cut there, at the first line break, and at 200 characters.
summarizeError :: SomeException -> Text
summarizeError =
  T.take 200 . T.strip . fst . T.breakOn "Request {" . T.takeWhile (/= '\n') . show

-- | Left = invalid configuration (not retried); Right = done, with an optional external reference.
executeAction :: ActionEnv -> DRFC.FeedbackAction -> Flow (Either Text (Maybe Text))
executeAction env action = case action.actionType of
  DRFC.REPORT_ISSUE_TO_BPP ->
    withParam "issueType" $ \issueType -> do
      void $ CallBPPInternal.reportIssue env.merchant.driverOfferApiKey env.merchant.driverOfferBaseUrl env.ride.bppRideId.getId issueType
      pure $ Right Nothing
  DRFC.CREATE_TICKET ->
    withParam "category" $ \category -> do
      let subCategory = param "subCategory"
          queue = fromMaybe env.riderConfig.kaptureConfig.queue (param "queue")
      Right . Just <$> createTicket category subCategory queue
  DRFC.L0_SENSITIVE_WORD_CHECK ->
    if IC.checkForLOFeedback env.riderConfig.sensitiveWords env.riderConfig.sensitiveWordsForExactMatch env.answer.text
      then do
        let queue = fromMaybe env.riderConfig.kaptureConfig.queue env.riderConfig.kaptureConfig.l0FeedbackQueue
        ticketId <- createTicket "Customer Feedback" (Just "L0 Feedback") queue
        publishSlack "L0 during-ride feedback"
        pure . Right $ Just ticketId
      else pure $ Right Nothing
  DRFC.SLACK_ALERT -> do
    publishSlack (fromMaybe "During-ride feedback alert" $ param "title")
    pure $ Right Nothing
  DRFC.NOTIFY_RIDER ->
    withParam "notificationKey" $ \notificationKey -> do
      Notify.dynamicNotifyPerson
        env.person
        (Notify.createNotificationReq notificationKey identity)
        EmptyDynamicParam
        (Notification.Entity Notification.Product env.person.id.getId ())
        env.booking.tripCategory
        []
        (Just env.booking.configInExperimentVersions)
        Nothing
      pure $ Right Nothing
  DRFC.SAFETY_ESCALATION -> do
    void $
      DSupport.safetyCheckSupport
        (env.person.id, env.merchant.id)
        DSupport.SafetyCheckSupportReq {bookingId = env.booking.id, isSafe = False, description = answerSummary}
    pure $ Right Nothing
  DRFC.TAG_RIDE ->
    withParam "tag" $ \tag -> do
      -- Re-read so tags written since this request started are not lost.
      mbRide <- QRide.findById env.ride.id
      let current = fromMaybe [] (mbRide >>= (.rideTags))
      unless (tag `elem` current) $ do
        QRide.updateRideTags env.ride.id (Just $ current <> [tag])
        clearEligibleQuestionsCache env.ride.id
      pure $ Right Nothing
  where
    params = action.params
    param :: A.FromJSON a => Text -> Maybe a
    param key = case params of
      Just (A.Object obj) -> case A.fromJSON <$> KM.lookup (AK.fromText key) obj of
        Just (A.Success v) -> Just v
        _ -> Nothing
      _ -> Nothing
    withParam :: A.FromJSON a => Text -> (a -> Flow (Either Text (Maybe Text))) -> Flow (Either Text (Maybe Text))
    withParam key f = maybe (pure . Left $ "Missing or invalid param: " <> key) f (param key)

    answerSummary =
      T.intercalate " | " . catMaybes $
        [ Just $ "Question: " <> env.config.questionKey,
          ("Options: " <>) . T.intercalate ", " <$> env.answer.selectedOptionKeys,
          ("Text: " <>) <$> env.answer.text
        ]

    createTicket category subCategory queue = do
      phoneNumber <- mapM decrypt env.person.mobileNumber
      let req =
            Ticket.CreateTicketReq
              { category,
                subCategory,
                issueId = Nothing,
                issueDescription = answerSummary,
                mediaFiles = Nothing,
                name = Just $ SLP.getName env.person,
                phoneNo = phoneNumber,
                personId = env.person.id.getId,
                classification = Ticket.CUSTOMER,
                rideDescription = Just $ SIVR.buildRideInfo env.ride env.person phoneNumber,
                disposition = env.riderConfig.kaptureConfig.disposition,
                queue,
                becknIssueId = Nothing,
                ticketContext = Just Ticket.FeedbackTicket,
                xyneChannelId = Nothing,
                xyneTicketBody = Nothing,
                xyneSenderName = Nothing
              }
      (resp, _) <- TTicket.createTicket env.booking.merchantId env.booking.merchantOperatingCityId req
      pure resp.ticketId

    publishSlack title = do
      slackConfig <- asks (.slackNotificationConfig)
      let description =
            T.unlines
              [ title,
                "Ride Id : " <> env.ride.id.getId,
                "Ride Short Id : " <> env.ride.shortId.getShortId,
                "Customer : " <> SLP.getName env.person,
                "Driver : " <> env.ride.driverName,
                "Service Tier : " <> show env.booking.vehicleServiceTierType,
                answerSummary
              ]
          message =
            TL.toStrict . TLE.decodeUtf8 . A.encode $
              A.object
                [ "version" A..= ("1.0" :: Text),
                  "source" A..= ("custom" :: Text),
                  "content" A..= A.object ["description" A..= description]
                ]
      L.runIO $ Slack.publishMessage slackConfig message
