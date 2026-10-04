{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.TdsDistribution where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Kernel.Utils.TH
import Servant
import Servant.Client

data CreateTdsBatchReq = CreateTdsBatchReq
  { financialYear :: Kernel.Prelude.Text,
    quarter :: TdsQuarter,
    folderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    uploadedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    files :: [TdsFileInput]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateTdsBatchReq where
  hideSecrets = Kernel.Prelude.identity

data CreateTdsBatchResp = CreateTdsBatchResp {batchId :: Kernel.Prelude.Text, uploads :: [TdsFileUpload], rejected :: [TdsRejectedFile]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsBatchListItem = TdsBatchListItem
  { batchId :: Kernel.Prelude.Text,
    financialYear :: Kernel.Prelude.Text,
    quarter :: TdsQuarter,
    status :: TdsBatchStatus,
    uploadedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    confirmedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    sentCount :: Kernel.Prelude.Int,
    deliveredCount :: Kernel.Prelude.Int,
    failedCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsBatchListResp = TdsBatchListResp {batches :: [TdsBatchListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsBatchResp = TdsBatchResp
  { batchId :: Kernel.Prelude.Text,
    financialYear :: Kernel.Prelude.Text,
    quarter :: TdsQuarter,
    folderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: TdsBatchStatus,
    totalFiles :: Kernel.Prelude.Int,
    parsedFiles :: Kernel.Prelude.Int,
    readyCount :: Kernel.Prelude.Int,
    driverCount :: Kernel.Prelude.Int,
    fleetOwnerCount :: Kernel.Prelude.Int,
    panNotFoundCount :: Kernel.Prelude.Int,
    invalidCount :: Kernel.Prelude.Int,
    uploadedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    validatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    confirmedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    confirmedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    completedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    pendingCount :: Kernel.Prelude.Int,
    sentCount :: Kernel.Prelude.Int,
    deliveredCount :: Kernel.Prelude.Int,
    failedCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsBatchStatus
  = DRAFT
  | VALIDATED
  | SENDING
  | COMPLETED
  | CANCELLED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsCertificateItem = TdsCertificateItem {quarter :: TdsQuarter, certificate :: Kernel.Prelude.Maybe TdsRecordItem}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsConfirmReq = TdsConfirmReq {confirmedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets TdsConfirmReq where
  hideSecrets = Kernel.Prelude.identity

data TdsDownloadUrlResp = TdsDownloadUrlResp {url :: Kernel.Prelude.Text, fileName :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsFailureReason
  = MISSING_EMAIL
  | ADDRESS_NOT_FOUND
  | MAILBOX_FULL
  | ATTACHMENT_TOO_LARGE
  | REJECTED
  | SUPPRESSED
  | TIMEOUT
  | SEND_ERROR
  | MISSING_PDF
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsFileFilter
  = FILTER_ALL
  | FILTER_READY
  | FILTER_ISSUES
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data TdsFileInput = TdsFileInput {fileName :: Kernel.Prelude.Text, sizeBytes :: Kernel.Prelude.Int, mimeType :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsFileIssue
  = INVALID_NAME
  | NOT_PDF
  | WRONG_QUARTER
  | WRONG_FY
  | TOO_LARGE
  | MISSING_UPLOAD
  | DUPLICATE_PAN
  | PAN_NOT_FOUND
  | AMBIGUOUS_PAN
  | ALREADY_SENT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsFileItem = TdsFileItem
  { fileId :: Kernel.Prelude.Text,
    fileName :: Kernel.Prelude.Text,
    pan :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    matchedName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    recipientType :: Kernel.Prelude.Maybe TdsRecipientType,
    status :: TdsFileStatus,
    issue :: Kernel.Prelude.Maybe TdsFileIssue
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsFileListResp = TdsFileListResp {files :: [TdsFileItem], totalCount :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsFileStatus
  = PENDING
  | READY
  | SKIPPED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsFileUpload = TdsFileUpload {fileId :: Kernel.Prelude.Text, fileName :: Kernel.Prelude.Text, uploadUrl :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsPersonCertificatesResp = TdsPersonCertificatesResp {personId :: Kernel.Prelude.Text, email :: Kernel.Prelude.Maybe Kernel.Prelude.Text, certificates :: [TdsCertificateItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsQuarter
  = Q1
  | Q2
  | Q3
  | Q4
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data TdsQuarterState
  = QS_UPCOMING
  | QS_READY_TO_UPLOAD
  | QS_IN_PROGRESS
  | QS_SENT
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsQuarterSummary = TdsQuarterSummary
  { quarter :: TdsQuarter,
    state :: TdsQuarterState,
    dueDate :: Data.Time.Day,
    latestBatchId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    uploadedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    sentCount :: Kernel.Prelude.Int,
    deliveredCount :: Kernel.Prelude.Int,
    failedCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsRecipientType
  = DRIVER
  | FLEET_OWNER
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsRecordFilter
  = REC_FILTER_ALL
  | REC_FILTER_DELIVERED
  | REC_FILTER_FAILED
  | REC_FILTER_PENDING
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data TdsRecordItem = TdsRecordItem
  { recordId :: Kernel.Prelude.Text,
    personId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    recipientType :: Kernel.Prelude.Maybe TdsRecipientType,
    pan :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fileName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: TdsRecordStatus,
    failureReason :: Kernel.Prelude.Maybe TdsFailureReason,
    attemptCount :: Kernel.Prelude.Int,
    lastAttemptAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    deliveredAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsRecordListResp = TdsRecordListResp {records :: [TdsRecordItem], totalCount :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsRecordStatus
  = REC_PENDING
  | REC_SENDING
  | REC_SENT
  | REC_DELIVERED
  | REC_FAILED
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsRejectedFile = TdsRejectedFile {fileId :: Kernel.Prelude.Text, fileName :: Kernel.Prelude.Text, issue :: TdsFileIssue}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TdsRetryReq = TdsRetryReq {email :: Kernel.Prelude.Maybe Kernel.Prelude.Text, saveToProfile :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets TdsRetryReq where
  hideSecrets = Kernel.Prelude.identity

data TdsSummaryResp = TdsSummaryResp {financialYear :: Kernel.Prelude.Text, quarters :: [TdsQuarterSummary]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("tdsDistribution" :> (PostTdsDistributionBatchHelper :<|> PostTdsDistributionBatchValidate :<|> GetTdsDistributionBatch :<|> GetTdsDistributionBatchFiles :<|> PostTdsDistributionBatchCancel :<|> PostTdsDistributionBatchConfirmHelper :<|> GetTdsDistributionBatchRecords :<|> PostTdsDistributionBatchRetryFailedHelper :<|> GetTdsDistributionBatches :<|> GetTdsDistributionSummary :<|> GetTdsDistributionPersonCertificates :<|> GetTdsDistributionRecordDownloadUrl :<|> PostTdsDistributionRecordRetryHelper))

type PostTdsDistributionBatch = ("batch" :> ReqBody ('[JSON]) CreateTdsBatchReq :> Post ('[JSON]) CreateTdsBatchResp)

type PostTdsDistributionBatchHelper = ("batch" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody ('[JSON]) CreateTdsBatchReq :> Post ('[JSON]) CreateTdsBatchResp)

type PostTdsDistributionBatchValidate = ("batch" :> Capture "batchId" Kernel.Prelude.Text :> "validate" :> Post ('[JSON]) TdsBatchResp)

type GetTdsDistributionBatch = ("batch" :> Capture "batchId" Kernel.Prelude.Text :> Get ('[JSON]) TdsBatchResp)

type GetTdsDistributionBatchFiles =
  ( "batch" :> Capture "batchId" Kernel.Prelude.Text :> "files" :> QueryParam "fileFilter" TdsFileFilter
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> Get ('[JSON]) TdsFileListResp
  )

type PostTdsDistributionBatchCancel = ("batch" :> Capture "batchId" Kernel.Prelude.Text :> "cancel" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostTdsDistributionBatchConfirm = ("batch" :> Capture "batchId" Kernel.Prelude.Text :> "confirm" :> ReqBody ('[JSON]) TdsConfirmReq :> Post ('[JSON]) TdsBatchResp)

type PostTdsDistributionBatchConfirmHelper =
  ( "batch" :> Capture "batchId" Kernel.Prelude.Text :> "confirm" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           TdsConfirmReq
      :> Post ('[JSON]) TdsBatchResp
  )

type GetTdsDistributionBatchRecords =
  ( "batch" :> Capture "batchId" Kernel.Prelude.Text :> "records" :> QueryParam "recordFilter" TdsRecordFilter
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> Get ('[JSON]) TdsRecordListResp
  )

type PostTdsDistributionBatchRetryFailed = ("batch" :> Capture "batchId" Kernel.Prelude.Text :> "retryFailed" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type PostTdsDistributionBatchRetryFailedHelper =
  ( "batch" :> Capture "batchId" Kernel.Prelude.Text :> "retryFailed" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type GetTdsDistributionBatches = ("batches" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get ('[JSON]) TdsBatchListResp)

type GetTdsDistributionSummary = ("summary" :> MandatoryQueryParam "financialYear" Kernel.Prelude.Text :> Get ('[JSON]) TdsSummaryResp)

type GetTdsDistributionPersonCertificates =
  ( "person" :> Capture "personId" Kernel.Prelude.Text :> "certificates" :> MandatoryQueryParam "financialYear" Kernel.Prelude.Text
      :> Get
           ('[JSON])
           TdsPersonCertificatesResp
  )

type GetTdsDistributionRecordDownloadUrl = ("record" :> Capture "recordId" Kernel.Prelude.Text :> "downloadUrl" :> Get ('[JSON]) TdsDownloadUrlResp)

type PostTdsDistributionRecordRetry = ("record" :> Capture "recordId" Kernel.Prelude.Text :> "retry" :> ReqBody ('[JSON]) TdsRetryReq :> Post ('[JSON]) TdsRecordItem)

type PostTdsDistributionRecordRetryHelper =
  ( "record" :> Capture "recordId" Kernel.Prelude.Text :> "retry" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           TdsRetryReq
      :> Post ('[JSON]) TdsRecordItem
  )

data TdsDistributionAPIs = TdsDistributionAPIs
  { postTdsDistributionBatch :: (Kernel.Prelude.Text -> CreateTdsBatchReq -> EulerHS.Types.EulerClient CreateTdsBatchResp),
    postTdsDistributionBatchValidate :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient TdsBatchResp),
    getTdsDistributionBatch :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient TdsBatchResp),
    getTdsDistributionBatchFiles :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (TdsFileFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient TdsFileListResp),
    postTdsDistributionBatchCancel :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    postTdsDistributionBatchConfirm :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> TdsConfirmReq -> EulerHS.Types.EulerClient TdsBatchResp),
    getTdsDistributionBatchRecords :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (TdsRecordFilter) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient TdsRecordListResp),
    postTdsDistributionBatchRetryFailed :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getTdsDistributionBatches :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient TdsBatchListResp),
    getTdsDistributionSummary :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient TdsSummaryResp),
    getTdsDistributionPersonCertificates :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient TdsPersonCertificatesResp),
    getTdsDistributionRecordDownloadUrl :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient TdsDownloadUrlResp),
    postTdsDistributionRecordRetry :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> TdsRetryReq -> EulerHS.Types.EulerClient TdsRecordItem)
  }

mkTdsDistributionAPIs :: (Client EulerHS.Types.EulerClient API -> TdsDistributionAPIs)
mkTdsDistributionAPIs tdsDistributionClient = (TdsDistributionAPIs {..})
  where
    postTdsDistributionBatch :<|> postTdsDistributionBatchValidate :<|> getTdsDistributionBatch :<|> getTdsDistributionBatchFiles :<|> postTdsDistributionBatchCancel :<|> postTdsDistributionBatchConfirm :<|> getTdsDistributionBatchRecords :<|> postTdsDistributionBatchRetryFailed :<|> getTdsDistributionBatches :<|> getTdsDistributionSummary :<|> getTdsDistributionPersonCertificates :<|> getTdsDistributionRecordDownloadUrl :<|> postTdsDistributionRecordRetry = tdsDistributionClient

data TdsDistributionUserActionType
  = POST_TDS_DISTRIBUTION_BATCH
  | POST_TDS_DISTRIBUTION_BATCH_VALIDATE
  | GET_TDS_DISTRIBUTION_BATCH
  | GET_TDS_DISTRIBUTION_BATCH_FILES
  | POST_TDS_DISTRIBUTION_BATCH_CANCEL
  | POST_TDS_DISTRIBUTION_BATCH_CONFIRM
  | GET_TDS_DISTRIBUTION_BATCH_RECORDS
  | POST_TDS_DISTRIBUTION_BATCH_RETRY_FAILED
  | GET_TDS_DISTRIBUTION_BATCHES
  | GET_TDS_DISTRIBUTION_SUMMARY
  | GET_TDS_DISTRIBUTION_PERSON_CERTIFICATES
  | GET_TDS_DISTRIBUTION_RECORD_DOWNLOAD_URL
  | POST_TDS_DISTRIBUTION_RECORD_RETRY
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum (''TdsFileFilter))

$(mkHttpInstancesForEnum (''TdsQuarter))

$(mkHttpInstancesForEnum (''TdsRecordFilter))

$(Data.Singletons.TH.genSingletons [(''TdsDistributionUserActionType)])
