module Domain.Action.UI.DriverOnboarding.HyperVergeWebhook where

import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.Status as Status
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DRC
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.HyperVergeVerification as DHVVerification
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.SharedLogic.HyperVerge.Error as HVErr
import qualified Kernel.External.Verification as KEV
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Utils.Common hiding (Error)
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Driver.OnBoarding as CQO
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.HyperVergeVerification as QHVVerification
import qualified Storage.Queries.Image as QImage
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Verification as Verification

type HyperVergeResultWebhookAPI =
  "hyperverge"
    :> "result"
    :> ReqBody '[JSON] Value
    :> Post '[JSON] AckResponse

data HyperVergeResultWebhookPayoad = HyperVergeResultWebhookPayoad
  { transactionId :: Text,
    applicationStatus :: Text,
    eventId :: Maybe Text,
    eventVersion :: Maybe Text,
    eventTime :: Maybe Text,
    eventType :: Maybe Text,
    reviewerEmail :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq, ToSchema)

hyperVergeResultWebhookHandler :: Value -> Flow AckResponse
hyperVergeResultWebhookHandler payload = do
  logDebug $ "HyperVerge Result Webhook Payload: " <> show payload
  parsedPayload <- case (fromJSON payload :: Result HyperVergeResultWebhookPayoad) of
    Error err -> throwError $ InvalidWebhookPayload "HyperVerge" (T.pack err)
    Success pyload -> return pyload
  when (isNothing parsedPayload.reviewerEmail) $ logError "Warning !!!!! Missing Field reviewerEmail in HyperVerge webhook Payload!!!!. Continuing the flow as other necessary fields are present."
  vstatus <- convertHVStatusToPanValidationStatus parsedPayload.applicationStatus
  QImage.updateVerificationStatus (Just vstatus) parsedPayload.reviewerEmail (Just parsedPayload.transactionId)
  imageEntity <- QImage.findByWrokflowTransactionId (Just parsedPayload.transactionId) >>= fromMaybeM (ImageNotFoundForWorkflowId parsedPayload.transactionId) . listToMaybe
  case imageEntity.imageType of
    DVC.PanCard -> do
      QDPC.updateVerificationStatus vstatus imageEntity.personId
      logInfo $ "PAN Card Validation Status Updated for Driver: " <> show imageEntity.personId <> " to: " <> show vstatus
    DVC.AadhaarCard -> do
      QAadhaarCard.updateVerificationStatus vstatus imageEntity.personId
      logInfo $ "Aadhaar Card Validation Status Updated for Driver: " <> show imageEntity.personId <> " to: " <> show vstatus
    DVC.ProfilePhoto ->
      logInfo $ "Profile Photo Validation Status Updated for Driver: " <> show imageEntity.personId <> " to: " <> show vstatus
    _ -> throwError $ InvalidImageType "HyperVerge" (show imageEntity.imageType) -- This should never happen as worlflowTransactionId is present only for above 3 doc types.
  return Ack
  where
    convertHVStatusToPanValidationStatus :: (MonadFlow m) => Text -> m Documents.VerificationStatus
    convertHVStatusToPanValidationStatus reviewStatus =
      case reviewStatus of
        "manually_approved" -> return Documents.VALID
        "manually_declined" -> return Documents.INVALID
        _ -> throwError $ InvalidReviewStatus "HyperVerge" reviewStatus

type HyperVergeVerificationWebhookAPI =
  "hyperverge"
    :> "verification"
    :> ReqBody '[JSON] Value
    :> Post '[JSON] AckResponse

data HyperVergeVerificationWebhookPayload = HypervVergeVerificationWebhookPayload
  { status :: Text,
    statusCode :: Int,
    result :: Maybe VerificationResultInfo
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq, ToSchema)

data VerificationResultInfo = VerificationResultInfo
  { message :: Maybe Text,
    id :: Maybe Text,
    endpoint :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq, ToSchema)

hyperVergeVerificaitonWebhookHandler :: Value -> Flow AckResponse
hyperVergeVerificaitonWebhookHandler payload = do
  logDebug $ "Received RC DL verification HyperVerge Webhook!!!!! Payload : " <> show payload
  parsedPayload <- case (fromJSON payload :: Result HyperVergeVerificationWebhookPayload) of
    Error err -> throwError $ InvalidWebhookPayload "HyperVerge" (T.pack err)
    Success pyload -> return pyload
  reqId <- fromMaybeM (InvalidWebhookPayload "HyperVerge" ("cannot resolve requestId for hyperverge verification payload. Payload : " <> show parsedPayload)) $ join (parsedPayload.result <&> (.id))
  verificationReq <- QHVVerification.findByRequestId reqId >>= fromMaybeM HyperVergeWebhookPayloadRecordNotFound
  mbRemPriorityList <- CQO.getVerificationPriorityList verificationReq.driverId
  when (verificationReq.status /= "pending") $ throwError DupilicateWebhookRecieved
  QHVVerification.updateStatus parsedPayload.status reqId
  regNum <- decrypt verificationReq.documentNumber
  apiEndpoint <- fromMaybeM (InvalidWebhookPayload "HyperVerge" ("cannot resolve endpoint for hyperverge verification payload. Payload : " <> show parsedPayload)) $ join (parsedPayload.result <&> (.endpoint))
  person <- QPerson.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
  case T.unpack apiEndpoint of
    "fetchDetailedRC" -> do
      rsp <- Verification.getTask person.merchantOperatingCityId KEV.HyperVergeRCDL $ KEV.GetTaskReq (Just "fetchDetailedRC") reqId
      case rsp of
        KEV.HyperVergeStatus resp -> do
          logDebug $ "RC: getTask api response for request id : " <> reqId <> " is : " <> show resp
          fromMaybeM (HVErr.HVError "status field empty in getTask response of HyperVerge") resp.status >>= \stat -> QHVVerification.updateResponse stat (Just $ show resp) reqId
          ack_ <- DRC.onVerifyRC person (Just $ makeVerificationReqRecord verificationReq) (resp {Verification.registrationNumber = Just regNum}) mbRemPriorityList Nothing Nothing verificationReq.multipleRC verificationReq.documentImageId1 verificationReq.retryCount (Just verificationReq.status)
          -- running statusHandler to enable Driver
          void $ Status.statusHandler (verificationReq.driverId, person.merchantId, person.merchantOperatingCityId) (Just True) verificationReq.multipleRC Nothing
          return ack_
        _ -> throwError $ InternalError "Invalid Provider !!!!!!!!"
    "checkDL" -> throwError $ InvalidWebhookPayload "HyperVerge" ("Payload contains invalid endpoint parameter value. Payload : " <> show parsedPayload)
    _ -> throwError $ InvalidWebhookPayload "HyperVerge" ("Payload contains invalid endpoint parameter value. Payload : " <> show parsedPayload)
  where
    makeVerificationReqRecord DHVVerification.HyperVergeVerification {..} =
      DRC.VerificationReqRecord
        { id = id.getId,
          verificaitonResponse = hypervergeResponse,
          ..
        }
