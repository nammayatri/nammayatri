-- | Runs the actions a feedback answer matched (rider-app holds what they need: the rider, Kapture,
-- Slack, notifications). For ride feedback the driver platform stores answers and evaluates the action
-- rules, and each result is reported back to it ('runActionsAndReport'); app feedback is stored here and
-- keeps its results itself ('runActionsInBackground').
module SharedLogic.RideFeedback.Actions
  ( ActionEnv (..),
    runActionsAndReport,
    runActionsInBackground,
    rideSubjectLines,
  )
where

import qualified API.Types.UI.RideFeedback as API
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
import SharedLogic.RideFeedback.Types (MatchedFeedbackAction (..), RideFeedbackActionResult (..), RideFeedbackActionStatus (..), RideFeedbackActionType (..))
import qualified SharedLogic.Scheduler.Jobs.SafetyCSAlert as SIVR
import qualified Slack.Flow as Slack
import qualified Tools.Notifications as Notify
import qualified Tools.Ticket as TTicket

data ActionEnv = ActionEnv
  { merchant :: DM.Merchant,
    riderConfig :: DRC.RiderConfig,
    person :: DP.Person,
    -- | The ride the answer is about; Nothing for booking-stage and app questions.
    ride :: Maybe DRide.Ride,
    -- | The booking the answer is about; Nothing for app questions.
    booking :: Maybe DB.Booking,
    -- | What the answer is about, one line each, for tickets and Slack alerts.
    subjectLines :: [Text],
    questionKey :: Text,
    answer :: API.RideFeedbackAnswer
  }

-- | Runs the actions in the background, then reports their results for the driver platform's response
-- (stored under its booking 'bppBookingId'). The driver platform already recorded them as PENDING, so a
-- lost run still leaves a trail to retry from.
runActionsAndReport :: ActionEnv -> Text -> Text -> [MatchedFeedbackAction] -> Flow ()
runActionsAndReport env bppBookingId responseId actions =
  runActionsInBackground env actions $ \results -> do
    res <-
      withTryCatch "RideFeedback:reportActionResults" $
        CallBPPInternal.rideFeedbackActionResults env.merchant.driverOfferApiKey env.merchant.driverOfferBaseUrl bppBookingId responseId results
    case res of
      Left err -> logError $ "RideFeedback: could not report action results for response " <> responseId <> ": " <> summarizeError err
      Right _ -> pure ()

-- | Runs the actions in the background and hands their results to 'saveResults'.
runActionsInBackground :: ActionEnv -> [MatchedFeedbackAction] -> ([RideFeedbackActionResult] -> Flow ()) -> Flow ()
runActionsInBackground env actions saveResults =
  unless (null actions) $
    fork "Feedback:actions" $ mapM (runAction env) actions >>= saveResults

-- | What a ride feedback answer is about, for tickets and Slack alerts.
rideSubjectLines :: DB.Booking -> Maybe DRide.Ride -> [Text]
rideSubjectLines booking ride =
  [ "Booking Id : " <> booking.id.getId,
    "Ride Id : " <> maybe "no ride yet" (.id.getId) ride,
    "Ride Short Id : " <> maybe "-" (.shortId.getShortId) ride,
    "Driver : " <> maybe "not assigned yet" (.driverName) ride,
    "Service Tier : " <> show booking.vehicleServiceTierType
  ]

mkResult :: UTCTime -> RideFeedbackActionStatus -> Int -> Maybe Text -> Maybe Text -> MatchedFeedbackAction -> RideFeedbackActionResult
mkResult now status attempts externalRef errorMessage action =
  RideFeedbackActionResult
    { ruleId = action.ruleId,
      actionType = action.actionType,
      status,
      attempts,
      externalRef,
      errorMessage,
      updatedAt = now
    }

-- | Runs an action once. None of them is idempotent (tickets, driver issue counters, notifications), and an
-- error can come after the side effect went through (e.g. Kapture timing out after creating the ticket), so
-- a failure is not retried here: it is reported as FAILED and retried on purpose from the dashboard.
runAction :: ActionEnv -> MatchedFeedbackAction -> Flow RideFeedbackActionResult
runAction env action = do
  res <- withTryCatch "RideFeedback:runAction" (executeAction env action)
  now <- getCurrentTime
  case res of
    Right (Right externalRef) -> pure $ mkResult now SUCCESS 1 externalRef Nothing action
    Right (Left configErr) -> pure $ mkResult now FAILED 1 Nothing (Just configErr) action
    Left err -> do
      let summary = summarizeError err
      logError $ "Feedback: action " <> show action.actionType <> " failed for question " <> env.questionKey <> " of rider " <> env.person.id.getId <> ": " <> summary
      pure $ mkResult now FAILED 1 Nothing (Just summary) action

-- | Short, secret-free description of a failure for storage and logs. HTTP client errors render
-- the whole request (including auth headers such as the BPP internal token) after "Request {",
-- so the text is cut there, at the first line break, and at 200 characters.
summarizeError :: SomeException -> Text
summarizeError =
  T.take 200 . T.strip . fst . T.breakOn "Request {" . T.takeWhile (/= '\n') . show

-- | Left = invalid configuration (not retried); Right = done, with an optional external reference.
executeAction :: ActionEnv -> MatchedFeedbackAction -> Flow (Either Text (Maybe Text))
executeAction env action = case action.actionType of
  REPORT_ISSUE_TO_BPP ->
    withParam "issueType" $ \issueType -> withRide $ \ride -> do
      void $ CallBPPInternal.reportIssue env.merchant.driverOfferApiKey env.merchant.driverOfferBaseUrl ride.bppRideId.getId issueType
      pure $ Right Nothing
  CREATE_TICKET ->
    withParam "category" $ \category -> do
      let subCategory = param "subCategory"
          queue = fromMaybe env.riderConfig.kaptureConfig.queue (param "queue")
      Right . Just <$> createTicket category subCategory queue
  L0_SENSITIVE_WORD_CHECK ->
    if IC.checkForLOFeedback env.riderConfig.sensitiveWords env.riderConfig.sensitiveWordsForExactMatch env.answer.text
      then do
        let queue = fromMaybe env.riderConfig.kaptureConfig.queue env.riderConfig.kaptureConfig.l0FeedbackQueue
        ticketId <- createTicket "Customer Feedback" (Just "L0 Feedback") queue
        -- The ticket is the action; the Slack alert is best-effort, so its failure never re-creates the ticket.
        slackRes <- withTryCatch "RideFeedback:l0Slack" $ publishSlack "L0 during-ride feedback"
        case slackRes of
          Left err -> logError $ "RideFeedback: L0 Slack alert failed for ticket " <> ticketId <> ": " <> summarizeError err
          Right () -> pure ()
        pure . Right $ Just ticketId
      else pure $ Right Nothing
  UnknownActionType name -> pure . Left $ "action type " <> name <> " is not supported by this rider-app yet"
  SLACK_ALERT -> do
    publishSlack (fromMaybe "During-ride feedback alert" $ param "title")
    pure $ Right Nothing
  NOTIFY_RIDER ->
    withParam "notificationKey" $ \notificationKey -> do
      Notify.dynamicNotifyPerson
        env.person
        (Notify.createNotificationReq notificationKey identity)
        EmptyDynamicParam
        (Notification.Entity Notification.Product env.person.id.getId ())
        (env.booking >>= (.tripCategory))
        []
        ((.configInExperimentVersions) <$> env.booking)
        Nothing
      pure $ Right Nothing
  SAFETY_ESCALATION -> withRide $ \_ -> withBooking $ \booking -> do
    void $
      DSupport.safetyCheckSupport
        (env.person.id, env.merchant.id)
        DSupport.SafetyCheckSupportReq {bookingId = booking.id, isSafe = False, description = answerSummary}
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
    -- The driver platform refuses these actions on booking-stage questions; this guards older configs.
    withRide f = maybe (pure . Left $ show action.actionType <> " needs a ride") f env.ride
    withBooking f = maybe (pure . Left $ show action.actionType <> " needs a booking") f env.booking

    answerSummary =
      T.intercalate " | " . catMaybes $
        [ Just $ "Question: " <> env.questionKey,
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
                rideDescription = (\ride -> SIVR.buildRideInfo ride env.person phoneNumber) <$> env.ride,
                disposition = env.riderConfig.kaptureConfig.disposition,
                queue,
                becknIssueId = Nothing,
                ticketContext = Just Ticket.FeedbackTicket,
                xyneChannelId = Nothing,
                xyneTicketBody = Nothing,
                xyneSenderName = Nothing,
                xyneExternalId = Nothing
              }
      (resp, _) <- TTicket.createTicket env.person.merchantId env.person.merchantOperatingCityId req
      pure resp.ticketId

    publishSlack title = do
      slackConfig <- asks (.slackNotificationConfig)
      let description =
            T.unlines $
              [title, "Customer : " <> SLP.getName env.person]
                <> env.subjectLines
                <> [answerSummary]
          message =
            TL.toStrict . TLE.decodeUtf8 . A.encode $
              A.object
                [ "version" A..= ("1.0" :: Text),
                  "source" A..= ("custom" :: Text),
                  "content" A..= A.object ["description" A..= description]
                ]
      L.runIO $ Slack.publishMessage slackConfig message
