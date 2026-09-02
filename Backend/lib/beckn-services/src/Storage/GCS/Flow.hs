module Storage.GCS.Flow
  ( get,
    put,
    putRaw,
    getRaw,
    delete,
    generateUploadUrl,
    generateDownloadUrl,
    headRequest,
    GcsClient,
    newGcsClient,
  )
where

import AWS.S3.Types (EntityTag (..), ObjectStatus (..))
import qualified Control.Concurrent.MVar as MV
import qualified Control.Exception as E
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime, defaultTimeLocale, formatTime, getCurrentTime)
import EulerHS.Prelude hiding (atomicWriteIORef, get, newIORef, put, readIORef)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common hiding (getCurrentTime, logInfo)
import Kernel.Utils.IOLogging (HasLog)
import Kernel.Utils.Logging (logInfo)
import Network.HTTP.Client
  ( Manager,
    RequestBody (..),
    defaultManagerSettings,
    httpLbs,
    method,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (Header, hAuthorization)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.URI.Encode as URIEncode
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

type TokenCache = MV.MVar (Maybe (Text, UTCTime))

data GcsEndpoints = GcsEndpoints
  { metadataServerUrl :: Text,
    gcsBaseUrl :: Text,
    gcsUploadBaseUrl :: Text,
    iamCredentialsBaseUrl :: Text
  }

data GcsClient = GcsClient
  { tokenCache :: TokenCache,
    tokenFailedUntil :: MV.MVar (Maybe UTCTime),
    serviceAccountEmail :: MV.MVar (Maybe Text),
    endpoints :: GcsEndpoints
  }

newGcsClient :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO GcsClient
newGcsClient mbMetadataUrl mbBaseUrl mbUploadBaseUrl mbIamUrl = do
  tokenCache <- MV.newMVar Nothing
  tokenFailedUntil <- MV.newMVar Nothing
  serviceAccountEmail <- MV.newMVar Nothing
  let baseUrl = T.dropWhileEnd (== '/') $ fromMaybe "https://storage.googleapis.com" mbBaseUrl
      endpoints =
        GcsEndpoints
          { metadataServerUrl = T.dropWhileEnd (== '/') $ fromMaybe "http://metadata.google.internal/computeMetadata/v1" mbMetadataUrl,
            gcsBaseUrl = baseUrl,
            gcsUploadBaseUrl = T.dropWhileEnd (== '/') $ fromMaybe (baseUrl <> "/upload/storage/v1") mbUploadBaseUrl,
            iamCredentialsBaseUrl = T.dropWhileEnd (== '/') $ fromMaybe "https://iamcredentials.googleapis.com" mbIamUrl
          }
  pure GcsClient {..}

tokenFailureBackoffSeconds :: NominalDiffTime
tokenFailureBackoffSeconds = 30

tokenFetchTimeoutUs :: Int
tokenFetchTimeoutUs = 5 * 1000000

getAccessToken :: (HasLog r, MonadReader r m, MonadIO m, Log m) => GcsClient -> m Text
getAccessToken client = liftIO $ do
  now <- getCurrentTime
  cached <- MV.readMVar client.tokenCache
  case cached of
    Just (tok, expiry) | now < expiry -> pure tok
    _ -> do
      failedUntil <- MV.readMVar client.tokenFailedUntil
      case failedUntil of
        Just retryAt | now < retryAt -> fail "[GCS] access token fetch failed recently, not retrying yet"
        _ -> do
          res <- E.try (fetchFreshToken client.endpoints)
          case res of
            Right (tok, ttlSeconds) -> do
              let expiry = addUTCTime (fromIntegral (ttlSeconds :: Int) - 60) now
              void $ MV.swapMVar client.tokenCache (Just (tok, expiry))
              void $ MV.swapMVar client.tokenFailedUntil Nothing
              pure tok
            Left (e :: E.SomeException) -> do
              void $ MV.swapMVar client.tokenFailedUntil (Just (addUTCTime tokenFailureBackoffSeconds now))
              E.throwIO e

fetchFreshToken :: GcsEndpoints -> IO (Text, Int)
fetchFreshToken eps = do
  res <- (Right <$> fetchFromMetadataServerWithTimeout eps) `E.catch` \(e :: E.SomeException) -> pure (Left e)
  case res of
    Right val -> pure val
    Left _ -> fetchFromGcloudADC

fetchFromMetadataServerWithTimeout :: GcsEndpoints -> IO (Text, Int)
fetchFromMetadataServerWithTimeout eps =
  timeout tokenFetchTimeoutUs (fetchFromMetadataServer eps) >>= \case
    Nothing -> fail "[GCS] metadata server token fetch timed out"
    Just r -> pure r

metadataGet :: GcsEndpoints -> String -> IO BL.ByteString
metadataGet eps suffix = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest $ T.unpack eps.metadataServerUrl <> suffix
  let req' = req {requestHeaders = [("Metadata-Flavor", "Google")]}
  resp <- httpLbs req' manager
  let s = statusCode $ responseStatus resp
  when (s < 200 || s >= 300) $
    fail $ "[GCS] Metadata server returned HTTP " <> show s
  pure $ responseBody resp

fetchFromMetadataServer :: GcsEndpoints -> IO (Text, Int)
fetchFromMetadataServer eps = do
  body <- metadataGet eps "/instance/service-accounts/default/token"
  case A.eitherDecode body of
    Left err -> fail $ "[GCS] Failed to parse metadata token: " <> err
    Right (obj :: A.Object) -> do
      tok <- case KM.lookup "access_token" obj of
        Just (A.String t) -> pure t
        _ -> fail "[GCS] 'access_token' missing from metadata response"
      ttl <- case KM.lookup "expires_in" obj of
        Just (A.Number n) -> pure $ floor n
        _ -> pure 3600
      pure (tok, ttl)

getServiceAccountEmail :: MonadIO m => GcsClient -> m Text
getServiceAccountEmail client =
  liftIO $
    MV.modifyMVar client.serviceAccountEmail $ \cached -> case cached of
      Just email -> pure (cached, email)
      Nothing -> do
        res <- timeout (10 * 1000000) $ metadataGet client.endpoints "/instance/service-accounts/default/email"
        case res of
          Nothing -> fail "[GCS] service account email lookup timed out after 10s"
          Just body -> do
            let email = T.strip $ TE.decodeUtf8 $ BL.toStrict body
            pure (Just email, email)

fetchFromGcloudADC :: IO (Text, Int)
fetchFromGcloudADC = do
  let timeoutUs = 10 * 1000000 -- 10 seconds timeout
  result <-
    timeout timeoutUs $
      readProcessWithExitCode "gcloud" ["auth", "application-default", "print-access-token"] ""
  case result of
    Nothing -> fail "[GCS] fetchFromGcloudADC: timed out after 10s"
    Just (ExitSuccess, out, _) -> pure (T.strip $ T.pack out, 3600)
    Just (ExitFailure c, out, err) ->
      fail $ "[GCS] fetchFromGcloudADC failed (exit " ++ show c ++ "): " ++ err ++ " | stdout: " ++ out

{-# NOINLINE gcsManagerRef #-}
gcsManagerRef :: IORef (Maybe Manager)
gcsManagerRef = unsafePerformIO $ newIORef Nothing

getGcsManager :: IO Manager
getGcsManager = do
  cached <- readIORef gcsManagerRef
  case cached of
    Just mgr -> pure mgr
    Nothing -> do
      mgr <- newManager tlsManagerSettings
      atomicWriteIORef gcsManagerRef (Just mgr)
      pure mgr

gcsRequest ::
  Text ->
  String ->
  String ->
  [Header] ->
  RequestBody ->
  IO BL.ByteString
gcsRequest token httpMethod url extraHeaders body = do
  manager <- getGcsManager
  initialRequest <- parseRequest url
  let allHeaders = (hAuthorization, "Bearer " <> TE.encodeUtf8 token) : ("Accept", "application/json") : extraHeaders
  let request =
        initialRequest
          { method = TE.encodeUtf8 $ T.pack httpMethod,
            requestHeaders = allHeaders,
            requestBody = body
          }
  resp <- httpLbs request manager
  let s = statusCode $ responseStatus resp
  when (s < 200 || s >= 300) $
    fail $
      "[GCS] HTTP error " ++ show s ++ " for " ++ httpMethod ++ " " ++ url
        ++ " — "
        ++ show (responseBody resp)
  pure $ responseBody resp

get ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasLog r) =>
  GcsClient ->
  Text ->
  String ->
  m Text
get client bucketName path = withLogTag "GCS" $ do
  logInfo $ "GET gs://" <> bucketName <> "/" <> T.pack path
  token <- getAccessToken client
  let url =
        T.unpack client.endpoints.gcsBaseUrl <> "/storage/v1/b/"
          <> T.unpack bucketName
          <> "/o/"
          <> urlEncodePath path
          <> "?alt=media"
  raw <- liftIO $ gcsRequest token "GET" url [] (RequestBodyLBS BL.empty)
  pure $ TE.decodeUtf8 $ BL.toStrict raw

getRaw ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasLog r) =>
  GcsClient ->
  Text ->
  String ->
  m BS.ByteString
getRaw client bucketName path = withLogTag "GCS" $ do
  logInfo $ "GET (raw) gs://" <> bucketName <> "/" <> T.pack path
  token <- getAccessToken client
  let url =
        T.unpack client.endpoints.gcsBaseUrl <> "/storage/v1/b/"
          <> T.unpack bucketName
          <> "/o/"
          <> urlEncodePath path
          <> "?alt=media"
  raw <- liftIO $ gcsRequest token "GET" url [] (RequestBodyLBS BL.empty)
  pure $ BL.toStrict raw

put ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasLog r) =>
  GcsClient ->
  Text ->
  String ->
  Text ->
  m ()
put client bucketName path content = withLogTag "GCS" $ do
  logInfo $ "PUT gs://" <> bucketName <> "/" <> T.pack path
  token <- getAccessToken client
  let url =
        T.unpack client.endpoints.gcsUploadBaseUrl <> "/b/"
          <> T.unpack bucketName
          <> "/o?uploadType=media&name="
          <> urlEncodePath path
  void $
    liftIO $
      gcsRequest
        token
        "POST"
        url
        [("Content-Type", "text/plain; charset=utf-8")]
        (RequestBodyBS $ TE.encodeUtf8 content)
  logInfo $ "PUT gs://" <> bucketName <> "/" <> T.pack path <> " — OK"

putRaw ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasLog r) =>
  GcsClient ->
  Text ->
  String ->
  BS.ByteString ->
  String ->
  m ()
putRaw client bucketName path bs contentType_ = withLogTag "GCS" $ do
  logInfo $ "PUT (raw) gs://" <> bucketName <> "/" <> T.pack path
  token <- getAccessToken client
  let url =
        T.unpack client.endpoints.gcsUploadBaseUrl <> "/b/"
          <> T.unpack bucketName
          <> "/o?uploadType=media&name="
          <> urlEncodePath path
  void $
    liftIO $
      gcsRequest
        token
        "POST"
        url
        [("Content-Type", TE.encodeUtf8 $ T.pack contentType_)]
        (RequestBodyBS bs)
  logInfo $ "PUT (raw) gs://" <> bucketName <> "/" <> T.pack path <> " — OK"

delete ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasLog r) =>
  GcsClient ->
  Text ->
  String ->
  m ()
delete client bucketName path = withLogTag "GCS" $ do
  logInfo $ "DELETE gs://" <> bucketName <> "/" <> T.pack path
  token <- getAccessToken client
  let url =
        T.unpack client.endpoints.gcsBaseUrl <> "/storage/v1/b/"
          <> T.unpack bucketName
          <> "/o/"
          <> urlEncodePath path
  void $ liftIO $ gcsRequest token "DELETE" url [] (RequestBodyLBS BL.empty)
  logInfo $ "DELETE gs://" <> bucketName <> "/" <> T.pack path <> " — OK"

generateUploadUrl ::
  (MonadFlow m, MonadReader r m, HasLog r) =>
  GcsClient ->
  Text ->
  String ->
  Seconds ->
  m Text
generateUploadUrl client bucketName path expires = withLogTag "GCS" $ do
  logInfo $ "Generating upload signed URL for gs://" <> bucketName <> "/" <> T.pack path
  generateSignedUrl client "PUT" bucketName path expires

generateDownloadUrl ::
  (MonadFlow m, MonadReader r m, HasLog r) =>
  GcsClient ->
  Text ->
  String ->
  Seconds ->
  m Text
generateDownloadUrl client bucketName path expires = withLogTag "GCS" $ do
  logInfo $ "Generating download signed URL for gs://" <> bucketName <> "/" <> T.pack path
  generateSignedUrl client "GET" bucketName path expires

headRequest ::
  (MonadFlow m, MonadReader r m, HasLog r) =>
  GcsClient ->
  Text ->
  String ->
  m ObjectStatus
headRequest client bucketName path = withLogTag "GCS" $ do
  logInfo $ "HEAD gs://" <> bucketName <> "/" <> T.pack path
  token <- getAccessToken client
  let url =
        T.unpack client.endpoints.gcsBaseUrl <> "/storage/v1/b/"
          <> T.unpack bucketName
          <> "/o/"
          <> urlEncodePath path
  raw <- liftIO $ gcsRequest token "GET" url [] (RequestBodyLBS BL.empty)
  case A.eitherDecode raw of
    Left err ->
      throwError $ InternalError $ "[GCS] headRequest: failed to parse metadata: " <> T.pack err
    Right (obj :: A.Object) -> do
      sizeBytes <- case KM.lookup "size" obj of
        Just (A.String s) ->
          case reads (T.unpack s) of
            [(n, "")] -> pure n
            _ -> throwError $ InternalError "[GCS] headRequest: 'size' is not a number"
        _ -> throwError $ InternalError "[GCS] headRequest: 'size' field missing"
      let eTag = case KM.lookup "etag" obj of
            Just (A.String e) -> e
            _ -> ""
      logInfo $ "HEAD gs://" <> bucketName <> "/" <> T.pack path <> " — size=" <> show sizeBytes
      pure $ ObjectStatus {fileSizeInBytes = sizeBytes, entityTag = EntityTag eTag}

generateSignedUrl ::
  (MonadFlow m, MonadReader r m, HasLog r) =>
  GcsClient ->
  Text ->
  Text ->
  String ->
  Seconds ->
  m Text
generateSignedUrl client httpMethod bucketName path expires = do
  email <- liftIO $ getServiceAccountEmail client
  token <- getAccessToken client
  now <- liftIO getCurrentTime
  let host = T.takeWhile (/= '/') $ T.replace "https://" "" $ T.replace "http://" "" client.endpoints.gcsBaseUrl
      datestamp = T.pack $ formatTime defaultTimeLocale "%Y%m%d" now
      timestamp = T.pack $ formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" now
      scope = datestamp <> "/auto/storage/goog4_request"
      credential = email <> "/" <> scope
      canonicalPath = "/" <> bucketName <> "/" <> T.pack (urlEncodePath path)
      queryParams =
        [ ("X-Goog-Algorithm", "GOOG4-RSA-SHA256"),
          ("X-Goog-Credential", credential),
          ("X-Goog-Date", timestamp),
          ("X-Goog-Expires", T.pack (show (fromIntegral expires :: Int))),
          ("X-Goog-SignedHeaders", "host")
        ]
      canonicalQuery = T.intercalate "&" [k <> "=" <> encodeQueryValue v | (k, v) <- queryParams]
      canonicalRequest =
        T.intercalate
          "\n"
          [httpMethod, canonicalPath, canonicalQuery, "host:" <> host, "", "host", "UNSIGNED-PAYLOAD"]
      stringToSign =
        T.intercalate
          "\n"
          ["GOOG4-RSA-SHA256", timestamp, scope, TE.decodeUtf8 $ B16.encode $ SHA256.hash $ TE.encodeUtf8 canonicalRequest]
  signature <- liftIO $ signBlob client token email (TE.encodeUtf8 stringToSign)
  pure $
    client.endpoints.gcsBaseUrl <> canonicalPath <> "?" <> canonicalQuery
      <> "&X-Goog-Signature="
      <> TE.decodeUtf8 (B16.encode signature)

signBlob :: GcsClient -> Text -> Text -> BS.ByteString -> IO BS.ByteString
signBlob client token email payload = do
  let url =
        T.unpack client.endpoints.iamCredentialsBaseUrl
          <> "/v1/projects/-/serviceAccounts/"
          <> T.unpack email
          <> ":signBlob"
      body = A.encode $ A.object ["payload" A..= TE.decodeUtf8 (B64.encode payload)]
  res <-
    timeout (10 * 1000000) $
      gcsRequest token "POST" url [("Content-Type", "application/json")] (RequestBodyLBS body)
  case res of
    Nothing -> fail "[GCS] signBlob timed out after 10s"
    Just raw -> case A.eitherDecode raw of
      Left err -> fail $ "[GCS] signBlob: failed to parse response: " <> err
      Right (obj :: A.Object) -> case KM.lookup "signedBlob" obj of
        Just (A.String b) -> either (\e -> fail $ "[GCS] signBlob: invalid signedBlob: " <> e) pure $ B64.decode (TE.encodeUtf8 b)
        _ -> fail "[GCS] signBlob: 'signedBlob' missing from response"

encodeQueryValue :: Text -> Text
encodeQueryValue = T.replace "/" "%2F" . T.pack . URIEncode.encode . T.unpack

urlEncodePath :: String -> String
urlEncodePath = URIEncode.encode
