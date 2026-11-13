module SharedLogic.DriverOnboarding.DocumentProtection
  ( DocumentProtectionFlow,
    readDocument,
    readDocumentIfAvailable,
    dropProcessingAccess,
    dropVehicleProcessingAccess,
    storeDocument,
  )
where

import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.Image as DImage
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleRegistrationCertificate as DRC
import EulerHS.Types (base64Decode, base64Encode)
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Documents (VerificationStatus (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.DocumentVault (DocumentVault, DocumentVaultError (..), isSealedObject)
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverGstin as QGstin
import qualified Storage.Queries.DriverLicense as QDL
import qualified Storage.Queries.DriverPanCard as QPan
import qualified Storage.Queries.DriverUdyam as QUdyam
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import Tools.Error

type DocumentProtectionFlow m r =
  ( MonadFlow m,
    MonadReader r m,
    HasField "s3Env" r (S3.S3Env m),
    HasField "documentVault" r (Maybe DocumentVault)
  )

isVerifiedAtSource :: DVC.DocumentType -> Maybe Text -> Bool
isVerifiedAtSource imageType workflowTransactionId =
  isJust workflowTransactionId && imageType `elem` [DVC.PanCard, DVC.AadhaarCard]

storeDocument :: (DocumentProtectionFlow m r, Metrics.CoreMetrics m) => Id DImage.Image -> DVC.DocumentType -> Maybe Text -> Text -> Text -> m (Maybe Text)
storeDocument imageId imageType workflowTransactionId path image = do
  mbVault <- asks (.documentVault)
  case mbVault of
    Just vault | vault.shouldSeal (show imageType) -> do
      document <- base64Decode image & fromMaybeM (InvalidRequest "Failed to decode base64 image")
      if isVerifiedAtSource imageType workflowTransactionId
        then do
          result <- liftIO . try $ vault.sealForArchive imageId.getId document
          object <- either vaultFailed pure result
          putSealedObject object
          pure Nothing
        else do
          result <- liftIO . try $ vault.sealForProcessing imageId.getId document
          sealed <- case result of
            Right sealed -> pure sealed
            Left (ProcessingKeyUnavailable reason) -> do
              Metrics.incrementGenericMetrics "document_vault_key_unavailable"
              throwError . InternalError $ "No document processing key is available: " <> reason
            Left err -> vaultFailed err
          putSealedObject sealed.object
          pure . Just $ base64Encode sealed.encryptedDataKey
    _ -> do
      S3.put (T.unpack path) image
      pure Nothing
  where
    putSealedObject object = S3.putRaw (T.unpack path) object "application/octet-stream"
    vaultFailed (err :: DocumentVaultError) = do
      Metrics.incrementGenericMetrics "document_vault_failure"
      throwError . InternalError $ show err

readDocument :: DocumentProtectionFlow m r => DImage.Image -> m Text
readDocument image = readDocumentIfAvailable image >>= fromMaybeM (DocumentArchived image.id.getId)

readDocumentIfAvailable :: DocumentProtectionFlow m r => DImage.Image -> m (Maybe Text)
readDocumentIfAvailable image = do
  object <- S3.getRaw (T.unpack image.s3Path)
  if isSealedObject object
    then openSealedDocument image object
    else pure . Just $ TE.decodeUtf8With TE.lenientDecode object

openSealedDocument :: DocumentProtectionFlow m r => DImage.Image -> ByteString -> m (Maybe Text)
openSealedDocument image object = case image.encryptedDataKey >>= base64Decode of
  Nothing -> pure Nothing
  Just dataKey -> do
    vault <- asks (.documentVault) >>= fromMaybeM (InternalError "Document vault is not configured")
    result <- liftIO . try $ vault.openWithProcessingKey image.id.getId dataKey object
    case result of
      Right document -> pure . Just $ base64Encode document
      Left ProcessingWindowClosed -> pure Nothing
      Left err -> throwError . InternalError $ show err

dropProcessingAccess :: (DocumentProtectionFlow m r, CacheFlow m r, EsqDBFlow m r) => DTC.TransporterConfig -> Id DP.Person -> m ()
dropProcessingAccess transporterConfig personId = do
  mbVault <- asks (.documentVault)
  when (isJust mbVault) $ do
    images <- filter (isJust . (.encryptedDataKey)) <$> ImageQuery.findAllByPersonId transporterConfig personId
    forM_ images $ \image ->
      whenM (isSettled image) $ ImageQuery.updateEncryptedDataKey Nothing image.id

dropVehicleProcessingAccess :: (DocumentProtectionFlow m r, CacheFlow m r, EsqDBFlow m r) => DRC.VehicleRegistrationCertificate -> m ()
dropVehicleProcessingAccess rc = do
  mbVault <- asks (.documentVault)
  when (isJust mbVault) $ do
    mbImage <- ImageQuery.findById rc.documentImageId
    whenJust (mfilter (isJust . (.encryptedDataKey)) mbImage) $ \image ->
      whenM (isSettled image) $ ImageQuery.updateEncryptedDataKey Nothing image.id

isSettled :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DImage.Image -> m Bool
isSettled image
  | image.verificationStatus == Just INVALID = pure True
  | otherwise = maybe False (`elem` [VALID, INVALID]) <$> documentRowStatus image

documentRowStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DImage.Image -> m (Maybe VerificationStatus)
documentRowStatus image = case image.imageType of
  DVC.DriverLicense -> fmap (.verificationStatus) <$> QDL.findByImageId image.id
  DVC.VehicleRegistrationCertificate -> fmap (.verificationStatus) <$> QRC.findByImageId image.id
  DVC.PanCard -> fmap (.verificationStatus) <$> QPan.findByImageId image.id
  DVC.GSTCertificate -> fmap (.verificationStatus) <$> QGstin.findByImageId image.id
  DVC.UDYAMCertificate -> fmap (.verificationStatus) <$> QUdyam.findByImageId image.id
  DVC.AadhaarCard -> do
    front <- QAadhaarCard.findByFrontImageId (Just image.id)
    back <- QAadhaarCard.findByBackImageId (Just image.id)
    pure $ (.verificationStatus) <$> (front <|> back)
  _ -> pure Nothing
