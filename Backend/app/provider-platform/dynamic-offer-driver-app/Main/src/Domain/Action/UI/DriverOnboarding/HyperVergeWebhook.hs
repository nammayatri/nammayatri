module Domain.Action.UI.DriverOnboarding.HyperVergeWebhook where

import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Types.DocumentVerificationConfig as DVC
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Utils.Common hiding (Error)
import Servant hiding (throwError)
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.Image as QImage
import Tools.Error

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
