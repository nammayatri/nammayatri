{-# OPTIONS_GHC -Wno-deprecations #-}

module SharedLogic.Scheduler.Jobs.SendLegalPolicyNotification where

import qualified Control.Exception as E
import qualified Dashboard.Common as Common
import qualified Data.Text as T
import qualified Domain.Types.EmailTemplate as DET
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PolicyAndComplianceDocument as DPCD
import qualified Domain.Types.RiderConfig as Fields
import qualified Email.Flow as Email
import Kernel.Beam.Functions (findAllWithOptionsKV)
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.External.Encryption (decrypt)
import Kernel.External.Types (Language (ENGLISH), SchedulerFlow)
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Sequelize as Se
import SharedLogic.InvoiceEmail (findEmailTemplate)
import SharedLogic.JobScheduler
import SharedLogic.LegalPolicyEmail (RenderedLegalPolicyEmail (..), renderLegalPolicyEmail)
import qualified Storage.Beam.Person as BeamP
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import Storage.Queries.OrphanInstances.Person ()
import qualified Storage.Queries.PolicyAndComplianceDocument as QPCD
import Tools.Error

defaultBatchSize :: Int -- Check in evaluation
defaultBatchSize = 1000

handleSendLegalPolicyNotification ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasFlowEnv m r '["emailServiceConfig" ::: Email.EmailServiceConfig],
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text],
    HasKafkaProducer r
  ) =>
  Job 'SendLegalPolicyNotification ->
  m ExecutionResult
handleSendLegalPolicyNotification Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
  policy <- QPCD.findByPrimaryKey jobData.policyDocId >>= fromMaybeM (InvalidRequest $ "PolicyDoc " <> jobData.policyDocId.getId <> " not found")
  if not policy.enabled
    then return Complete
    else do
      riderConfig <-
        getConfig (RiderConfigDimensions {merchantOperatingCityId = jobData.merchantOperatingCityId.getId}) Nothing
          >>= fromMaybeM (RiderConfigDoesNotExist jobData.merchantOperatingCityId.getId)
      let batchSize = fromMaybe defaultBatchSize riderConfig.legalPolicyNotificationBatchSize
      recipients <-
        findAllWithOptionsKV
          [Se.Is BeamP.merchantId $ Se.Eq jobData.merchantId.getId]
          (Se.Asc BeamP.createdAt)
          (Just batchSize)
          (Just jobData.pageOffset)
      if null recipients
        then return Complete
        else do
          mbFallbackTemplate <- findEmailTemplate jobData.merchantOperatingCityId DET.LEGAL_POLICY_UPDATE ENGLISH
          case mbFallbackTemplate of
            Nothing -> do
              logError $ "LEGAL_POLICY_UPDATE email template not configured for city " <> jobData.merchantOperatingCityId.getId <> " — aborting batch"
              return Complete
            Just _ -> do
              emailServiceConfig <- asks (.emailServiceConfig)
              forM_ recipients $ sendOne emailServiceConfig policy jobData
              createJobIn @_ @'SendLegalPolicyNotification
                (Just jobData.merchantId)
                (Just jobData.merchantOperatingCityId)
                1
                (jobData {pageOffset = jobData.pageOffset + batchSize})
              return Complete

sendOne ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasKafkaProducer r,
    MonadReader r m
  ) =>
  Email.EmailServiceConfig ->
  DPCD.PolicyAndComplianceDocument ->
  SendLegalPolicyNotificationJobData ->
  DP.Person ->
  m ()
sendOne emailServiceConfig policy jobData person = do
  mbRecipientEmail <- mapM decrypt person.email
  case mbRecipientEmail of
    Nothing -> emitEvent person policy jobData AttemptFailed (Just "no_email") (Just "person has no email on file")
    Just recipientEmail -> do
      mbTemplate <- findEmailTemplate jobData.merchantOperatingCityId DET.LEGAL_POLICY_UPDATE (fromMaybe ENGLISH person.language)
      case mbTemplate of
        Nothing -> emitEvent person policy jobData AttemptFailed (Just "template_not_found") (Just "no LEGAL_POLICY_UPDATE email template for city")
        Just template -> case template.config of
          DET.LegalPolicyEmailTemplateConfig config -> dispatchEmail emailServiceConfig recipientEmail template config person policy jobData
          DET.InvoiceEmailTemplateConfig _ -> emitEvent person policy jobData AttemptFailed (Just "template_config_invalid") (Just $ "email template " <> template.id.getId <> " has no legal policy config")

dispatchEmail ::
  ( MonadFlow m,
    HasKafkaProducer r,
    MonadReader r m
  ) =>
  Email.EmailServiceConfig ->
  Text ->
  DET.EmailTemplate ->
  DET.LegalPolicyEmailConfig ->
  DP.Person ->
  DPCD.PolicyAndComplianceDocument ->
  SendLegalPolicyNotificationJobData ->
  m ()
dispatchEmail emailServiceConfig recipientEmail template config person policy jobData = do
  let rendered = renderLegalPolicyEmail template config policy
  unless (null rendered.missingKeys) $
    logWarning $ "Legal policy email template has unfilled keys: " <> T.intercalate "," rendered.missingKeys
  sendResult <- liftIO $ E.try @E.SomeException $ Email.sendPlainEmail emailServiceConfig config.fromEmail [recipientEmail] rendered.subject rendered.html Email.HtmlText
  case sendResult of
    Right () -> emitEvent person policy jobData AttemptedOk Nothing Nothing
    Left sendErr -> emitEvent person policy jobData AttemptFailed (Just "send_exception") (Just . show $ sendErr)

data AttemptStatus = AttemptedOk | AttemptFailed

statusText :: AttemptStatus -> Text
statusText AttemptedOk = "attempted_ok"
statusText AttemptFailed = "attempted_failed"

data LegalNotificationEvent = LegalNotificationEvent
  { event_id :: Text,
    occurred_at :: UTCTime,
    merchant_id :: Text,
    merchant_operating_city_id :: Text,
    policy_doc_id :: Text,
    policy_type :: Text,
    entity_type :: Text,
    policy_version :: Text,
    person_id :: Text,
    channel :: Text,
    status :: Text,
    error_code :: Maybe Text,
    error_message :: Maybe Text,
    batch_id :: Text
  }
  deriving (Generic, Show, ToJSON)

emitEvent ::
  (MonadFlow m, HasKafkaProducer r, MonadReader r m) =>
  DP.Person ->
  DPCD.PolicyAndComplianceDocument ->
  SendLegalPolicyNotificationJobData ->
  AttemptStatus ->
  Maybe Text ->
  Maybe Text ->
  m ()
emitEvent person policy jobData attemptStatus errorCode errorMessage = do
  now <- getCurrentTime
  let event =
        LegalNotificationEvent
          { event_id = person.id.getId <> ":" <> policy.id.getId,
            occurred_at = now,
            merchant_id = jobData.merchantId.getId,
            merchant_operating_city_id = jobData.merchantOperatingCityId.getId,
            policy_doc_id = policy.id.getId,
            policy_type = show policy.policyType,
            entity_type = show (fromMaybe Common.CustomerLegal policy.entityType),
            policy_version = policy.version,
            person_id = person.id.getId,
            channel = "email",
            status = statusText attemptStatus,
            error_code = errorCode,
            error_message = errorMessage,
            batch_id = jobData.batchId
          }
  pushToKafka event "legal-notification-events" person.id.getId -- push to kafka with person id
