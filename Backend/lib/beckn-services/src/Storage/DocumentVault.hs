module Storage.DocumentVault
  ( DocumentVault (..),
    DocumentVaultConfig (..),
    DocumentVaultError (..),
    SealedDocument (..),
    buildDocumentVault,
    isSealedObject,
  )
where

import qualified Amazonka
import qualified Amazonka.Auth as AmazonkaAuth
import Amazonka.Env (env_region)
import qualified Amazonka.KMS as KMS
import qualified Amazonka.KMS.Lens as KMS
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Lens ((.~), (?~), (^.), (^?))
import Control.Monad.Trans.Resource (runResourceT)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEAD, AEADMode (AEAD_GCM), AuthTag (..), aeadInit, aeadSimpleDecrypt, aeadSimpleEncrypt, cipherInit)
import Crypto.Error (eitherCryptoError)
import Crypto.Hash.Algorithms (SHA256 (..))
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.OAEP as OAEP
import Crypto.Random (getRandomBytes)
import Crypto.Store.X509 (readPubKeyFileFromMemory)
import qualified Data.Aeson as A
import Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (Day, addDays)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.X509 as X509
import Kernel.Prelude
import Kernel.Utils.Dhall (FromDhall)
import Servant.Client (Scheme (Https))

data DocumentVaultConfig = DocumentVaultConfig
  { region :: Text,
    processingKeyAliasPrefix :: Text,
    processingWindowDays :: Integer,
    processingRoleArn :: Text,
    keyAdminRoleArn :: Text,
    archiveKeyArn :: Text,
    kmsEndpoint :: Maybe BaseUrl,
    sealNewUploads :: Bool,
    documentTypes :: [Text]
  }
  deriving (Generic, FromDhall)

data SealedDocument = SealedDocument
  { object :: ByteString,
    encryptedDataKey :: ByteString
  }

data DocumentVault = DocumentVault
  { shouldSeal :: Text -> Bool,
    sealForProcessing :: Text -> ByteString -> IO SealedDocument,
    sealForArchive :: Text -> ByteString -> IO ByteString,
    openWithProcessingKey :: Text -> ByteString -> ByteString -> IO ByteString
  }

data DocumentVaultError
  = ProcessingWindowClosed
  | ProcessingKeyUnavailable Text
  | VaultFailure Text
  deriving (Show)

instance Exception DocumentVaultError

data ObjectParts = ObjectParts
  { authenticatedHeader :: ByteString,
    nonce :: ByteString,
    authTag :: ByteString,
    ciphertext :: ByteString
  }

objectMagic :: ByteString
objectMagic = BS.pack [0x00, 0x4e, 0x59, 0x44, 0x56, 0x01]

dekSizeBytes :: Int
dekSizeBytes = 32

nonceSizeBytes :: Int
nonceSizeBytes = 12

authTagSizeBytes :: Int
authTagSizeBytes = 16

processingKeyPurpose :: Text
processingKeyPurpose = "ny-onboarding-document-processing"

processingWindowClosedCodes :: [Amazonka.ErrorCode]
processingWindowClosedCodes =
  [ "DisabledException",
    "KMSInvalidStateException",
    "NotFoundException"
  ]

isSealedObject :: ByteString -> Bool
isSealedObject = BS.isPrefixOf objectMagic

buildDocumentVault :: DocumentVaultConfig -> IO DocumentVault
buildDocumentVault cfg = do
  envRef <- newIORef Nothing
  archiveKeyRef <- newIORef Nothing
  keyCreationLock <- newMVar ()
  let kmsEnv = cachedKmsEnv cfg envRef
      archiveKey env = cachedArchiveKey env cfg archiveKeyRef
  pure
    DocumentVault
      { shouldSeal = \documentType -> cfg.sealNewUploads && documentType `elem` cfg.documentTypes,
        sealForProcessing = \documentId document -> kmsEnv >>= \env -> archiveKey env >>= \key -> sealWithProcessingKey env cfg keyCreationLock key documentId document,
        sealForArchive = \documentId document -> kmsEnv >>= archiveKey >>= \key -> sealWithArchiveKeyOnly key documentId document,
        openWithProcessingKey = \documentId dataKey object -> kmsEnv >>= \env -> openObject env documentId dataKey object
      }

cachedKmsEnv :: DocumentVaultConfig -> IORef (Maybe Amazonka.Env) -> IO Amazonka.Env
cachedKmsEnv cfg envRef =
  readIORef envRef >>= \case
    Just env -> pure env
    Nothing -> do
      env <- try (buildKmsEnv cfg) >>= either (\(err :: SomeException) -> throwIO . VaultFailure $ "KMS client unavailable: " <> show err) pure
      writeIORef envRef (Just env)
      pure env

cachedArchiveKey :: Amazonka.Env -> DocumentVaultConfig -> IORef (Maybe RSA.PublicKey) -> IO RSA.PublicKey
cachedArchiveKey env cfg archiveKeyRef =
  readIORef archiveKeyRef >>= \case
    Just key -> pure key
    Nothing -> do
      key <- fetchArchiveKey env cfg
      writeIORef archiveKeyRef (Just key)
      pure key

fetchArchiveKey :: Amazonka.Env -> DocumentVaultConfig -> IO RSA.PublicKey
fetchArchiveKey env cfg = do
  response <- runResourceT (Amazonka.sendEither env $ KMS.newGetPublicKey cfg.archiveKeyArn) >>= either (throwIO . VaultFailure . show) pure
  unless (response ^. KMS.getPublicKeyResponse_keyId == Just cfg.archiveKeyArn) $
    throwIO . VaultFailure $ "Archive key id mismatch: " <> show (response ^. KMS.getPublicKeyResponse_keyId)
  unless (response ^. KMS.getPublicKeyResponse_keyUsage == Just KMS.KeyUsageType_ENCRYPT_DECRYPT) $
    throwIO $ VaultFailure "Archive key is not an encryption key"
  unless (response ^. KMS.getPublicKeyResponse_keySpec == Just KMS.KeySpec_RSA_4096) $
    throwIO $ VaultFailure "Archive key is not RSA_4096"
  der <- maybe (throwIO $ VaultFailure "Archive key has no public key") pure (response ^. KMS.getPublicKeyResponse_publicKey)
  either (throwIO . VaultFailure) pure $ parseArchiveKey der

buildKmsEnv :: DocumentVaultConfig -> IO Amazonka.Env
buildKmsEnv cfg = case cfg.kmsEndpoint of
  Nothing -> withRegion <$> Amazonka.newEnv Amazonka.discover
  Just endpoint -> do
    noAuthEnv <- Amazonka.newEnvNoAuth
    pure . withRegion . Amazonka.configureService (endpointService endpoint) $
      AmazonkaAuth.fromKeys (Amazonka.AccessKey "local") (Amazonka.SecretKey "local") noAuthEnv
  where
    withRegion = env_region .~ fromString (T.unpack cfg.region)
    endpointService endpoint =
      Amazonka.setEndpoint
        (baseUrlScheme endpoint == Https)
        (TE.encodeUtf8 . T.pack $ baseUrlHost endpoint)
        (baseUrlPort endpoint)
        KMS.defaultService

parseArchiveKey :: ByteString -> Either Text RSA.PublicKey
parseArchiveKey der = case readPubKeyFileFromMemory pem of
  [X509.PubKeyRSA key] -> Right key
  _ -> Left "Archive key is not an RSA public key"
  where
    pem = "-----BEGIN PUBLIC KEY-----\n" <> BS.intercalate "\n" (chunksOf64 $ convertToBase Base64 der) <> "\n-----END PUBLIC KEY-----\n"
    chunksOf64 bytes
      | BS.null bytes = []
      | otherwise = let (line, rest) = BS.splitAt 64 bytes in line : chunksOf64 rest

documentContext :: Text -> HM.HashMap Text Text
documentContext documentId = HM.singleton "documentId" documentId

sealWithProcessingKey :: Amazonka.Env -> DocumentVaultConfig -> MVar () -> RSA.PublicKey -> Text -> ByteString -> IO SealedDocument
sealWithProcessingKey env cfg keyCreationLock archiveKey documentId document = do
  response <- generateProcessingKey env cfg keyCreationLock documentId
  sealedBytes <- sealObject archiveKey documentId (response ^. KMS.generateDataKeyResponse_plaintext) document
  pure SealedDocument {object = sealedBytes, encryptedDataKey = response ^. KMS.generateDataKeyResponse_ciphertextBlob}

generateProcessingKey :: Amazonka.Env -> DocumentVaultConfig -> MVar () -> Text -> IO KMS.GenerateDataKeyResponse
generateProcessingKey env cfg keyCreationLock documentId = do
  today <- utctDay <$> getCurrentTime
  let alias = processingKeyAlias cfg today
  requestDataKey alias >>= \case
    Right response -> pure response
    Left err
      | hasErrorCode "NotFoundException" err ->
        withMVar keyCreationLock $ \_ ->
          requestDataKey alias >>= \case
            Right response -> pure response
            Left retryErr
              | hasErrorCode "NotFoundException" retryErr -> do
                keyRef <- createProcessingKey env cfg today
                requestDataKey keyRef >>= either (throwIO . ProcessingKeyUnavailable . show) pure
              | otherwise -> throwIO . VaultFailure $ show retryErr
      | otherwise -> throwIO . VaultFailure $ show err
  where
    requestDataKey alias =
      runResourceT . Amazonka.sendEither env $
        KMS.newGenerateDataKey alias
          & KMS.generateDataKey_keySpec ?~ KMS.DataKeySpec_AES_256
          & KMS.generateDataKey_encryptionContext ?~ documentContext documentId

createProcessingKey :: Amazonka.Env -> DocumentVaultConfig -> Day -> IO Text
createProcessingKey env cfg day = do
  let alias = processingKeyAlias cfg day
      deadline = processingDeadline cfg day
      request =
        KMS.newCreateKey
          & KMS.createKey_keySpec ?~ KMS.KeySpec_SYMMETRIC_DEFAULT
          & KMS.createKey_keyUsage ?~ KMS.KeyUsageType_ENCRYPT_DECRYPT
          & KMS.createKey_description ?~ ("Onboarding document processing key for " <> formatDay day)
          & KMS.createKey_policy ?~ processingKeyPolicy cfg deadline
          & KMS.createKey_bypassPolicyLockoutSafetyCheck ?~ True
          & KMS.createKey_tags
            ?~ [ KMS.newTag "purpose" processingKeyPurpose,
                 KMS.newTag "processingDay" (formatDay day),
                 KMS.newTag "expiresAt" (formatInstant deadline)
               ]
  created <- runResourceT (Amazonka.sendEither env request) >>= either (throwIO . ProcessingKeyUnavailable . ("CreateKey failed: " <>) . show) pure
  keyId <- maybe (throwIO $ ProcessingKeyUnavailable "CreateKey returned no key metadata") (pure . (^. KMS.keyMetadata_keyId)) (created ^. KMS.createKeyResponse_keyMetadata)
  runResourceT (Amazonka.sendEither env $ KMS.newCreateAlias alias keyId) >>= \case
    Right _ -> pure keyId
    Left err
      | hasErrorCode "AlreadyExistsException" err -> pure alias
      | otherwise -> throwIO . ProcessingKeyUnavailable $ "CreateAlias failed: " <> show err

processingDeadline :: DocumentVaultConfig -> Day -> UTCTime
processingDeadline cfg day = UTCTime (addDays (cfg.processingWindowDays + 1) day) 0

processingKeyPolicy :: DocumentVaultConfig -> UTCTime -> Text
processingKeyPolicy cfg deadline =
  TE.decodeUtf8 . BSL.toStrict . A.encode $
    A.object
      [ "Version" A..= ("2012-10-17" :: Text),
        "Statement"
          A..= [ A.object
                   [ "Sid" A..= ("KeyAdministrationWithoutDataAccess" :: Text),
                     "Effect" A..= ("Allow" :: Text),
                     "Principal" A..= A.object ["AWS" A..= cfg.keyAdminRoleArn],
                     "Action"
                       A..= ( [ "kms:DescribeKey",
                                "kms:GetKeyPolicy",
                                "kms:ListResourceTags",
                                "kms:EnableKey",
                                "kms:DisableKey",
                                "kms:ScheduleKeyDeletion",
                                "kms:CancelKeyDeletion",
                                "kms:DeleteAlias",
                                "kms:TagResource",
                                "kms:UntagResource"
                              ] ::
                                [Text]
                            ),
                     "Resource" A..= ("*" :: Text)
                   ],
                 A.object
                   [ "Sid" A..= ("ProcessingUntilDeadline" :: Text),
                     "Effect" A..= ("Allow" :: Text),
                     "Principal" A..= A.object ["AWS" A..= cfg.processingRoleArn],
                     "Action" A..= (["kms:GenerateDataKey", "kms:Decrypt"] :: [Text]),
                     "Resource" A..= ("*" :: Text),
                     "Condition"
                       A..= A.object
                         [ "DateLessThan" A..= A.object ["aws:CurrentTime" A..= formatInstant deadline],
                           "ForAllValues:StringEquals" A..= A.object ["kms:EncryptionContextKeys" A..= (["documentId"] :: [Text])],
                           "Null" A..= A.object ["kms:EncryptionContextKeys" A..= ("false" :: Text)]
                         ]
                   ]
               ]
      ]

formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

formatInstant :: UTCTime -> Text
formatInstant = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

processingKeyAlias :: DocumentVaultConfig -> Day -> Text
processingKeyAlias cfg day = cfg.processingKeyAliasPrefix <> formatDay day

sealWithArchiveKeyOnly :: RSA.PublicKey -> Text -> ByteString -> IO ByteString
sealWithArchiveKeyOnly archiveKey documentId document = do
  dek <- getRandomBytes dekSizeBytes
  sealObject archiveKey documentId dek document

sealObject :: RSA.PublicKey -> Text -> ByteString -> ByteString -> IO ByteString
sealObject archiveKey documentId dek document = do
  archiveEncryptedDataKey <- OAEP.encrypt (OAEP.defaultOAEPParams SHA256) archiveKey dek >>= either (throwIO . VaultFailure . show) pure
  iv <- getRandomBytes nonceSizeBytes
  let header = objectMagic <> lengthPrefixed archiveEncryptedDataKey <> lengthPrefixed (TE.encodeUtf8 documentId) <> iv
  cipherState <- either (throwIO . VaultFailure) pure $ initCipher dek iv
  let (tag, encrypted) = aeadSimpleEncrypt cipherState header document authTagSizeBytes
  pure $ header <> BA.convert (unAuthTag tag) <> encrypted

openObject :: Amazonka.Env -> Text -> ByteString -> ByteString -> IO ByteString
openObject env documentId dataKey sealedBytes = do
  parts <- maybe (throwIO $ VaultFailure "Sealed object is malformed") pure $ parseObject sealedBytes
  let request = KMS.newDecrypt dataKey & KMS.decrypt_encryptionContext ?~ documentContext documentId
  response <- runResourceT (Amazonka.sendEither env request) >>= either (throwIO . classifyKmsError) pure
  dek <- maybe (throwIO $ VaultFailure "KMS Decrypt returned no plaintext") pure (response ^. KMS.decryptResponse_plaintext)
  cipherState <- either (throwIO . VaultFailure) pure $ initCipher dek parts.nonce
  maybe (throwIO $ VaultFailure "Sealed object failed authentication") pure $
    aeadSimpleDecrypt cipherState parts.authenticatedHeader parts.ciphertext (AuthTag $ BA.convert parts.authTag)

classifyKmsError :: Amazonka.Error -> DocumentVaultError
classifyKmsError err
  | any (`hasErrorCode` err) processingWindowClosedCodes = ProcessingWindowClosed
  | otherwise = VaultFailure (show err)

hasErrorCode :: Amazonka.ErrorCode -> Amazonka.Error -> Bool
hasErrorCode code err = (err ^? Amazonka._ServiceError . Amazonka.serviceError_code) == Just code

initCipher :: ByteString -> ByteString -> Either Text (AEAD AES256)
initCipher dek iv =
  either (Left . show) Right . eitherCryptoError $ do
    cipher <- cipherInit dek
    aeadInit AEAD_GCM (cipher :: AES256) iv

lengthPrefixed :: ByteString -> ByteString
lengthPrefixed field = BS.pack [fromIntegral (size `shiftR` 8), fromIntegral size] <> field
  where
    size = BS.length field

takeLengthPrefixed :: ByteString -> Maybe (ByteString, ByteString)
takeLengthPrefixed bytes = case BS.unpack (BS.take 2 bytes) of
  [high, low] -> do
    let size = (fromIntegral high `shiftL` 8) .|. fromIntegral low
        (field, rest) = BS.splitAt size (BS.drop 2 bytes)
    if BS.length field == size then Just (field, rest) else Nothing
  _ -> Nothing

parseObject :: ByteString -> Maybe ObjectParts
parseObject sealedBytes = do
  afterMagic <- BS.stripPrefix objectMagic sealedBytes
  (_, afterArchiveLock) <- takeLengthPrefixed afterMagic
  (_, afterDocumentId) <- takeLengthPrefixed afterArchiveLock
  let (iv, afterIv) = BS.splitAt nonceSizeBytes afterDocumentId
      (tag, encrypted) = BS.splitAt authTagSizeBytes afterIv
      header = BS.take (BS.length sealedBytes - BS.length afterIv) sealedBytes
  if BS.length iv == nonceSizeBytes && BS.length tag == authTagSizeBytes
    then Just ObjectParts {authenticatedHeader = header, nonce = iv, authTag = tag, ciphertext = encrypted}
    else Nothing
