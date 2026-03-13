module Storage.GCS.Flow
  ( get,
    put,
    putRaw,
    delete,
    generateUploadUrl,
    generateDownloadUrl,
    headRequest,
    TokenCache,
    newTokenCache,
  )
where

import AWS.S3.Types (EntityTag (..), ObjectStatus (..))
import qualified Control.Exception as E
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Network.URI.Encode as URIEncode
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import EulerHS.Prelude hiding (atomicWriteIORef, get, newIORef, put, readIORef)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common hiding (getCurrentTime)
import Kernel.Utils.IOLogging (HasLog)
import Network.HTTP.Client
  ( RequestBody (..),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    method,
    requestBody,
    requestHeaders,
    responseBody,
    responseStatus,
    Manager,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (Header, hAuthorization)
import Network.HTTP.Types.Status (statusCode)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import System.Exit (ExitCode(..))
import qualified Control.Concurrent.MVar as MV

type TokenCache = MV.MVar (Maybe (Text, UTCTime))

newTokenCache :: IO TokenCache
newTokenCache = MV.newMVar Nothing

getAccessToken :: (HasLog r, MonadReader r m, MonadIO m, Log m) => TokenCache -> m Text
getAccessToken cache = do
  liftIO $ MV.modifyMVar cache $ \cached -> do
    now <- getCurrentTime
    case cached of
      Just (tok, expiry) | now < expiry -> pure (cached, tok)
      _ -> do
        (tok, ttlSeconds) <- fetchFreshToken
        let expiry = addUTCTime (fromIntegral (ttlSeconds :: Int) - 60) now
        pure (Just (tok, expiry), tok)

fetchFreshToken :: IO (Text, Int)
fetchFreshToken = do
  res <- (Right <$> fetchFromMetadataServer) `E.catch` \(e :: E.SomeException) -> pure (Left e)
  case res of
    Right val -> pure val
    Left _ -> do
      fetchFromGcloudADC

fetchFromMetadataServer :: IO (Text, Int)
fetchFromMetadataServer = do
  manager <- newManager defaultManagerSettings
  req <-
    parseRequest
      "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token"
  let req' = req {requestHeaders = [("Metadata-Flavor", "Google")]}
  resp <- httpLbs req' manager
  let s = statusCode $ responseStatus resp
  when (s < 200 || s >= 300) $
    fail $ "[GCS] Metadata server returned HTTP " <> show s
  case A.eitherDecode (responseBody resp) of
    Left err -> fail $ "[GCS] Failed to parse metadata token: " <> err
    Right (obj :: A.Object) -> do
      tok <- case KM.lookup "access_token" obj of
        Just (A.String t) -> pure t
        _ -> fail "[GCS] 'access_token' missing from metadata response"
      ttl <- case KM.lookup "expires_in" obj of
        Just (A.Number n) -> pure $ floor n
        _ -> pure 3600
      pure (tok, ttl)

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

gcsBaseUrl :: Text
gcsBaseUrl = "https://storage.googleapis.com"

gcsUploadBaseUrl :: Text
gcsUploadBaseUrl = "https://storage.googleapis.com/upload/storage/v1"


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
  req <- parseRequest url
  let req' =
        req
          { method = TE.encodeUtf8 $ T.pack httpMethod,
            requestHeaders =
              (hAuthorization, "Bearer " <> TE.encodeUtf8 token)
                : ("Accept", "application/json")
                : extraHeaders,
            requestBody = body
          }
  resp <- httpLbs req' manager
  let s = statusCode $ responseStatus resp
  when (s < 200 || s >= 300) $
    fail $
      "[GCS] HTTP error " ++ show s ++ " for " ++ httpMethod ++ " " ++ url
        ++ " — " ++ show (responseBody resp)
  pure $ responseBody resp

get ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasLog r) =>
  TokenCache -> Text -> String -> m Text
get cache bucketName path = withLogTag "GCS" $ do
  logInfo $ "GET gs://" <> bucketName <> "/" <> T.pack path
  token <- getAccessToken cache
  let url =
        T.unpack gcsBaseUrl <> "/storage/v1/b/"
          <> T.unpack bucketName <> "/o/"
          <> urlEncodePath path <> "?alt=media"
  raw <- liftIO $ gcsRequest token "GET" url [] (RequestBodyLBS BL.empty)
  pure $ TE.decodeUtf8 $ BL.toStrict raw

put ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasLog r) =>
  TokenCache -> Text -> String -> Text -> m ()
put cache bucketName path content = withLogTag "GCS" $ do
  logInfo $ "PUT gs://" <> bucketName <> "/" <> T.pack path
  token <- getAccessToken cache
  let url =
        T.unpack gcsUploadBaseUrl <> "/b/"
          <> T.unpack bucketName <> "/o?uploadType=media&name="
          <> urlEncodePath path
  void $
    liftIO $
      gcsRequest
        token "POST" url
        [("Content-Type", "text/plain; charset=utf-8")]
        (RequestBodyBS $ TE.encodeUtf8 content)
  logInfo $ "PUT gs://" <> bucketName <> "/" <> T.pack path <> " — OK"

putRaw ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasLog r) =>
  TokenCache -> Text -> String -> BS.ByteString -> String -> m ()
putRaw cache bucketName path bs contentType_ = withLogTag "GCS" $ do
  logInfo $ "PUT (raw) gs://" <> bucketName <> "/" <> T.pack path
  token <- getAccessToken cache
  let url =
        T.unpack gcsUploadBaseUrl <> "/b/"
          <> T.unpack bucketName <> "/o?uploadType=media&name="
          <> urlEncodePath path
  void $
    liftIO $
      gcsRequest
        token "POST" url
        [("Content-Type", TE.encodeUtf8 $ T.pack contentType_)]
        (RequestBodyBS bs)
  logInfo $ "PUT (raw) gs://" <> bucketName <> "/" <> T.pack path <> " — OK"

delete ::
  (CoreMetrics m, MonadFlow m, MonadReader r m, HasLog r) =>
  TokenCache -> Text -> String -> m ()
delete cache bucketName path = withLogTag "GCS" $ do
  logInfo $ "DELETE gs://" <> bucketName <> "/" <> T.pack path
  token <- getAccessToken cache
  let url =
        T.unpack gcsBaseUrl <> "/storage/v1/b/"
          <> T.unpack bucketName <> "/o/"
          <> urlEncodePath path
  void $ liftIO $ gcsRequest token "DELETE" url [] (RequestBodyLBS BL.empty)
  logInfo $ "DELETE gs://" <> bucketName <> "/" <> T.pack path <> " — OK"

generateUploadUrl ::
  (MonadFlow m) =>
  Text -> String -> Seconds -> m Text
generateUploadUrl bucketName path expires = withLogTag "GCS" $ do
  logInfo $ "Generating upload signed URL for gs://" <> bucketName <> "/" <> T.pack path
  generateSignedUrl "PUT" bucketName path expires

generateDownloadUrl ::
  (MonadFlow m) =>
  Text -> String -> Seconds -> m Text
generateDownloadUrl bucketName path expires = withLogTag "GCS" $ do
  logInfo $ "Generating download signed URL for gs://" <> bucketName <> "/" <> T.pack path
  generateSignedUrl "GET" bucketName path expires

headRequest ::
  (MonadFlow m, MonadReader r m, HasLog r) =>
  TokenCache -> Text -> String -> m ObjectStatus
headRequest cache bucketName path = withLogTag "GCS" $ do
  logInfo $ "HEAD gs://" <> bucketName <> "/" <> T.pack path
  token <- getAccessToken cache
  let url =
        T.unpack gcsBaseUrl <> "/storage/v1/b/"
          <> T.unpack bucketName <> "/o/"
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

generateSignedUrl :: (MonadFlow m) => String -> Text -> String -> Seconds -> m Text
generateSignedUrl httpMethod bucketName path expires = do
  let gcsUri = "gs://" <> T.unpack bucketName <> "/" <> path
      expiresStr = show (fromIntegral expires :: Int)
      timeoutUs = 60 * 1000000 :: Int

  result <- liftIO $
    timeout timeoutUs $
      readProcessWithExitCode
        "gcloud"
        [ "storage", "sign-url",
          "--duration=" <> expiresStr <> "s",
          "--http-verb=" <> httpMethod,
          "--format=value(signed_url)",
          gcsUri
        ]
        ""

  case result of
    Nothing -> throwError $ InternalError $ "[GCS] generateSignedUrl: timed out after 60s"
    Just (ExitSuccess, out, _) ->
      case T.lines (T.strip $ T.pack out) of
        (u : _) -> pure u
        [] -> throwError $ InternalError "[GCS] generateSignedUrl: gcloud returned empty output"
    Just (ExitFailure c, out, err) ->
      throwError $ InternalError $ "[GCS] generateSignedUrl failed (exit " <> T.pack (show c) <> "): " <> T.pack err <> " | stdout: " <> T.pack out

urlEncodePath :: String -> String
urlEncodePath = URIEncode.encode