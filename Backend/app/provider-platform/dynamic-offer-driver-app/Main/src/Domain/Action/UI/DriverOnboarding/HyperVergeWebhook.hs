module Domain.Action.UI.DriverOnboarding.HyperVergeWebhook where

import Control.Lens ((^?), _head)
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DDL
import qualified Domain.Action.UI.DriverOnboarding.Status as Status
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DRC
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Environment
import Kernel.External.Encryption
import qualified Kernel.External.Verification as KEV
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Utils.Common hiding (Error)
import Servant hiding (throwError)
import qualified SharedLogic.DriverOnboarding as SLogicOnboarding
import qualified Storage.CachedQueries.Driver.OnBoarding as CQO
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
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
  imageEntity <- QImage.findByWrokflowTransactionId (Just parsedPayload.transactionId) >>= fromMaybeM (ImageNotFoundForWorkflowId parsedPayload.transactionId) . (^? _head)
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
    :> BasicAuth "username-password" BasicAuthData
    :> ReqBody '[JSON] Value
    :> Post '[JSON] AckResponse

data HyperVergeVerificationWebhookPayload = HyperVergeVerificationWebhookPayload
  { status :: Text,
    statusCode :: Text,
    result :: Maybe VerificationResultInfo
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq, ToSchema)

data VerificationResultInfo = VerificationResultInfo
  { message :: Maybe Text,
    id :: Maybe Text,
    endpoint :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, Show, Eq, ToSchema)

hyperVergeVerificaitonWebhookHandler :: BasicAuthData -> Value -> Flow AckResponse
hyperVergeVerificaitonWebhookHandler authData payload = do
  logDebug $ "Received RC DL verification HyperVerge Webhook!!!!! Payload : " <> show payload
  hvServiceConfig <- CQMSC.findOne (DMSC.VerificationService KEV.HyperVergeRCDL) >>= fromMaybeM (MerchantServiceConfigNotFound "default" "Verification" $ show KEV.HyperVergeRCDL)
  (username, password) <- case hvServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig (KEV.HyperVergeVerificationConfigRCDL vsc) -> do
      passwd <- decrypt vsc.password
      return (vsc.username, passwd)
    _ -> throwError $ InternalError "Unknown Service Config"
  unless (basicAuthUsername authData == DT.encodeUtf8 username && basicAuthPassword authData == DT.encodeUtf8 password) $ throwError WebhookAuthFailed
  parsedPayload <- case (fromJSON payload :: Result HyperVergeVerificationWebhookPayload) of
    Error err -> throwError $ InvalidWebhookPayload "HyperVerge" (T.pack err)
    Success pyload -> return pyload
  reqId <- fromMaybeM (InvalidWebhookPayload "HyperVerge" ("cannot resolve requestId for hyperverge verification payload. Payload : " <> show parsedPayload)) $ join (parsedPayload.result <&> (.id))
  verificationReq <- QHVVerification.findByRequestId reqId >>= fromMaybeM (HyperVergeWebhookPayloadRecordNotFound reqId)
  mbRemPriorityList <- CQO.getVerificationPriorityList verificationReq.driverId
  when (verificationReq.status /= "pending" && verificationReq.status /= "source_down_retrying") $ throwError DuplicateWebhookReceived
  QHVVerification.updateStatus parsedPayload.status reqId
  regNum <- decrypt verificationReq.documentNumber
  apiEndpoint <- fromMaybeM (InvalidWebhookPayload "HyperVerge" ("cannot resolve endpoint for hyperverge verification payload. Payload : " <> show parsedPayload)) $ join (parsedPayload.result <&> (.endpoint))
  person <- QPerson.findById verificationReq.driverId >>= fromMaybeM (PersonNotFound verificationReq.driverId.getId)
  case T.unpack apiEndpoint of
    "RCVerification" -> do
      rsp <- Verification.getTask person.merchantOperatingCityId KEV.HyperVergeRCDL (KEV.GetTaskReq (Just "RCVerification") reqId) QHVVerification.updateResponse
      case rsp of
        KEV.RCResp resp -> do
          logDebug $ "RC: getTask api response for request id : " <> reqId <> " is : " <> show resp
          ack_ <- DRC.onVerifyRC person (Just $ SLogicOnboarding.makeHVVerificationReqRecord verificationReq) (resp {Verification.registrationNumber = Just regNum}) mbRemPriorityList Nothing Nothing verificationReq.documentImageId1 verificationReq.retryCount (Just verificationReq.status) (Just KEV.HyperVergeRCDL)
          -- running statusHandler to enable Driver
          let onlyMandatoryDocs = Just True
          void $ Status.statusHandler (verificationReq.driverId, person.merchantId, person.merchantOperatingCityId) (Just True) Nothing Nothing (Just False) onlyMandatoryDocs Nothing
          return ack_
        _ -> throwError $ InternalError "Document and apiEndpoint mismatch occurred !!!!!!!!"
    "checkDL" -> do
      rsp <- Verification.getTask person.merchantOperatingCityId KEV.HyperVergeRCDL (KEV.GetTaskReq (Just "checkDL") reqId) QHVVerification.updateResponse
      case rsp of
        KEV.DLResp resp -> do
          logDebug $ "DL: getTask api response for request id : " <> reqId <> " is : " <> show resp
          ack_ <- DDL.onVerifyDL (SLogicOnboarding.makeHVVerificationReqRecord verificationReq) resp KEV.HyperVergeRCDL
          -- running statusHandler to enable Driver
          let onlyMandatoryDocs = Just True
          void $ Status.statusHandler (verificationReq.driverId, person.merchantId, person.merchantOperatingCityId) (Just True) Nothing Nothing (Just False) onlyMandatoryDocs Nothing
          return ack_
        _ -> throwError $ InternalError "Document and apiEndpoint mismatch occurred !!!!!!!!"
    _ -> throwError $ InvalidWebhookPayload "HyperVerge" ("Payload contains invalid endpoint parameter value. Payload : " <> show parsedPayload)
