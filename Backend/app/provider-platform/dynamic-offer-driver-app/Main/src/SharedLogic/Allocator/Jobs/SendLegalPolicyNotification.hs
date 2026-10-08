{-# OPTIONS_GHC -Wno-deprecations #-}

module SharedLogic.Allocator.Jobs.SendLegalPolicyNotification where

import qualified Control.Exception as E
import qualified Dashboard.Common as Common
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PolicyAndComplianceDocument as DPCD
import qualified Email.Flow as Email
import Kernel.Beam.Functions (findAllWithOptionsKV)
import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified Sequelize as Se
import SharedLogic.Allocator
import SharedLogic.LegalPolicyEmail (LegalPolicyEmailLogicInput (..), LegalPolicyEmailLogicOutput (..))
import qualified Storage.Beam.FleetOwnerInformation as BeamFOI
import qualified Storage.Beam.Person as BeamP
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import Storage.Queries.OrphanInstances.FleetOwnerInformation ()
import Storage.Queries.OrphanInstances.Person ()
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PolicyAndComplianceDocument as QPCD
import Tools.DynamicLogic (getAppDynamicLogic)

defaultBatchSize :: Int
defaultBatchSize = 1000

defaultFromAddress :: Text
defaultFromAddress = "noreply@moving.tech"

handleSendLegalPolicyNotification ::
  ( CacheFlow m r,
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
      transporterConfig <-
        getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = jobData.merchantOperatingCityId.getId}) Nothing
          >>= fromMaybeM (TransporterConfigNotFound jobData.merchantOperatingCityId.getId)
      let batchSize = fromMaybe defaultBatchSize (transporterConfig.limitsConfig >>= (.legalPolicyNotificationBatchSize))
      recipients <- fetchRecipients jobData batchSize
      if null recipients
        then return Complete
        else do
          now <- getCurrentTime
          (dynamicLogics, _mbVersion) <- getAppDynamicLogic (cast jobData.merchantOperatingCityId) LYT.LEGAL_POLICY_UPDATE_EMAIL now Nothing Nothing
          if null dynamicLogics
            then do
              logError $ "LEGAL_POLICY_UPDATE_EMAIL dynamic logic not configured for city " <> jobData.merchantOperatingCityId.getId <> " — aborting batch"
              return Complete
            else do
              emailServiceConfig <- asks (.emailServiceConfig)
              forM_ recipients $ sendOne emailServiceConfig policy jobData dynamicLogics
              createJobIn @_ @'SendLegalPolicyNotification
                (Just jobData.merchantId)
                (Just jobData.merchantOperatingCityId)
                1
                (jobData {pageOffset = jobData.pageOffset + batchSize})
              return Complete

fetchRecipients ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  SendLegalPolicyNotificationJobData ->
  Int ->
  m [DP.Person]
fetchRecipients jobData batchSize = case jobData.entityType of
  -- TODO @dhruv-1010: audit BPP Person roles once role model is firmed up and
  -- widen/narrow this filter accordingly. For now DriverLegal targets DRIVER + BUS_DRIVER only.
  Common.DriverLegal ->
    findAllWithOptionsKV
      [ Se.And
          [ Se.Is BeamP.merchantId $ Se.Eq jobData.merchantId.getId,
            Se.Is BeamP.role $ Se.In [DP.DRIVER, DP.BUS_DRIVER]
          ]
      ]
      (Se.Asc BeamP.createdAt)
      (Just batchSize)
      (Just jobData.pageOffset)
  Common.FleetOwnerLegal -> do
    fleetOwners <-
      findAllWithOptionsKV
        [Se.Is BeamFOI.merchantId $ Se.Eq jobData.merchantId.getId]
        (Se.Asc BeamFOI.createdAt)
        (Just batchSize)
        (Just jobData.pageOffset)
    QPerson.findAllByPersonIdsAndMerchantId jobData.merchantId (map (.fleetOwnerPersonId.getId) fleetOwners)
  Common.CustomerLegal -> pure []

sendOne ::
  ( MonadFlow m,
    HasKafkaProducer r,
    MonadReader r m
  ) =>
  Email.EmailServiceConfig ->
  DPCD.PolicyAndComplianceDocument ->
  SendLegalPolicyNotificationJobData ->
  [A.Value] ->
  DP.Person ->
  m ()
sendOne emailServiceConfig policy jobData dynamicLogics person =
  case person.email of
    Nothing -> emitEvent person policy jobData AttemptFailed (Just "no_email") (Just "person has no email on file")
    Just recipientEmail -> do
      logicResp <- LYTU.runLogics dynamicLogics (buildLogicInput policy jobData person)
      case A.fromJSON logicResp.result :: A.Result LegalPolicyEmailLogicOutput of
        A.Error parseErr -> emitEvent person policy jobData AttemptFailed (Just "logic_result_parse_failed") (Just . T.pack $ parseErr)
        A.Success emailContent -> dispatchEmail emailServiceConfig recipientEmail emailContent person policy jobData

dispatchEmail ::
  ( MonadFlow m,
    HasKafkaProducer r,
    MonadReader r m
  ) =>
  Email.EmailServiceConfig ->
  Text ->
  LegalPolicyEmailLogicOutput ->
  DP.Person ->
  DPCD.PolicyAndComplianceDocument ->
  SendLegalPolicyNotificationJobData ->
  m ()
dispatchEmail emailServiceConfig recipientEmail emailContent person policy jobData = do
  let fromAddress = fromMaybe defaultFromAddress emailContent.fromEmail
      subject = T.concat emailContent.subject
      body = T.concat emailContent.body
  sendResult <- liftIO $ E.try @E.SomeException $ Email.sendPlainEmail emailServiceConfig fromAddress [recipientEmail] subject body Email.HtmlText
  case sendResult of
    Right () -> emitEvent person policy jobData AttemptedOk Nothing Nothing
    Left sendErr -> emitEvent person policy jobData AttemptFailed (Just "send_exception") (Just . show $ sendErr)

buildLogicInput :: DPCD.PolicyAndComplianceDocument -> SendLegalPolicyNotificationJobData -> DP.Person -> LegalPolicyEmailLogicInput
buildLogicInput policy jobData person =
  LegalPolicyEmailLogicInput
    { policyDocId = policy.id,
      merchantId = jobData.merchantId,
      merchantOperatingCityId = jobData.merchantOperatingCityId,
      policyType = policy.policyType,
      entityType = policy.entityType,
      version = policy.version,
      url = policy.url,
      isMandatory = policy.isMandatory,
      personId = person.id,
      language = person.language
    }

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
            entity_type = show jobData.entityType,
            policy_version = policy.version,
            person_id = person.id.getId,
            channel = "email",
            status = statusText attemptStatus,
            error_code = errorCode,
            error_message = errorMessage,
            batch_id = jobData.batchId
          }
  pushToKafka event "legal-notification-events" person.id.getId
