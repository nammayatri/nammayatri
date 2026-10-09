-- | Runs the actions the driver platform matched for an answer (it stores answers and evaluates the
-- action rules; rider-app holds what the actions need: the rider, Kapture, Slack, notifications) and
-- reports each result back to the driver platform, where it is kept with the answer.
module SharedLogic.RideFeedback.Actions
  ( ActionEnv (..),
    runActionsAndReport,
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
    ride :: DRide.Ride,
    booking :: DB.Booking,
    questionKey :: Text,
    answer :: API.RideFeedbackAnswer
  }

-- | Runs the actions in the background, then reports their results for the driver platform's response.
-- The driver platform already recorded them as PENDING, so a lost run still leaves a trail to retry from.
runActionsAndReport :: ActionEnv -> Text -> [MatchedFeedbackAction] -> Flow ()
runActionsAndReport env responseId actions =
  unless (null actions) $
    fork "RideFeedback:actions" $ do
      results <- mapM (runAction env) actions
      res <-
        withTryCatch "RideFeedback:reportActionResults" $
          CallBPPInternal.rideFeedbackActionResults env.merchant.driverOfferApiKey env.merchant.driverOfferBaseUrl env.ride.bppRideId.getId responseId results
      case res of
        Left err -> logError $ "RideFeedback: could not report action results for response " <> responseId <> ": " <> summarizeError err
        Right _ -> pure ()

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

maxAttempts :: Int
maxAttempts = 3

-- | Config errors (bad params) fail at once; runtime exceptions are retried with a growing delay.
runAction :: ActionEnv -> MatchedFeedbackAction -> Flow RideFeedbackActionResult
runAction env action = go 1
  where
    go attempt = do
      res <- withTryCatch "RideFeedback:runAction" (executeAction env action)
      now <- getCurrentTime
      case res of
        Right (Right externalRef) -> pure $ mkResult now SUCCESS attempt externalRef Nothing action
        Right (Left configErr) -> pure $ mkResult now FAILED attempt Nothing (Just configErr) action
        Left err
          | attempt < maxAttempts -> threadDelaySec (Seconds attempt) >> go (attempt + 1)
          | otherwise -> do
            let summary = summarizeError err
            logError $ "RideFeedback: action " <> show action.actionType <> " failed for question " <> env.questionKey <> " on ride " <> env.ride.id.getId <> ": " <> summary
            pure $ mkResult now FAILED attempt Nothing (Just summary) action

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
    withParam "issueType" $ \issueType -> do
      void $ CallBPPInternal.reportIssue env.merchant.driverOfferApiKey env.merchant.driverOfferBaseUrl env.ride.bppRideId.getId issueType
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
        publishSlack "L0 during-ride feedback"
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
        env.booking.tripCategory
        []
        (Just env.booking.configInExperimentVersions)
        Nothing
      pure $ Right Nothing
  SAFETY_ESCALATION -> do
    void $
      DSupport.safetyCheckSupport
        (env.person.id, env.merchant.id)
        DSupport.SafetyCheckSupportReq {bookingId = env.booking.id, isSafe = False, description = answerSummary}
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
