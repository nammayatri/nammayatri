-- | Executes a declared chat lookup (IssueApiIntegration): substitutes
-- {{placeholders}} into the URL / headers / body, performs the HTTP call with
-- the integration's own timeout, and extracts the declared response fields by
-- dot-path. Follows the raw http-client precedent of
-- Kernel.External.MasterCloudForward because the URL *path* is runtime data,
-- which the Servant-client pattern cannot express.
module IssueManagement.Tools.ApiIntegrationExecutor
  ( ExecutionResult (..),
    executeIntegration,
    substituteTemplate,
    extractByPath,
  )
where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as V
import EulerHS.Prelude
import Kernel.Utils.Common (Log, logError)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as HttpTLS
import qualified Network.HTTP.Types as HttpTypes

data ExecutionResult = ExecutionResult
  { statusCode :: Maybe Int,
    latencyMs :: Int,
    -- | The declared response fields, extracted by dot-path. 'Nothing' for a
    -- field whose path did not resolve.
    extractedFields :: Map.Map Text (Maybe A.Value),
    rawResponse :: Maybe A.Value,
    errorMessage :: Maybe Text
  }

-- | Replace every {{name}} in the template with its value from the parameter
-- map. Unknown placeholders are left untouched so the caller can detect them.
substituteTemplate :: Map.Map Text Text -> Text -> Text
substituteTemplate params template =
  Map.foldrWithKey (\k v acc -> T.replace ("{{" <> k <> "}}") v acc) template params

-- | Follow a dot-path ("data.ride.status") into a JSON value. Supports
-- numeric segments as array indices ("items.0.id").
extractByPath :: Text -> A.Value -> Maybe A.Value
extractByPath path = go (T.splitOn "." path)
  where
    go [] v = Just v
    go (seg : rest) (A.Object obj) = AKM.lookup (AK.fromText seg) obj >>= go rest
    go (seg : rest) (A.Array arr) = do
      idx <- readMaybe (T.unpack seg)
      (arr V.!? idx) >>= go rest
    go _ _ = Nothing

-- | Run one integration. Never throws: every failure mode is folded into
-- 'errorMessage' so callers can branch to their fallback path.
executeIntegration ::
  (MonadIO m, Log m) =>
  -- | GET / POST as text ("GET" | "POST")
  Text ->
  -- | URL template
  Text ->
  -- | header (key, value-template) pairs
  [(Text, Text)] ->
  -- | body template (POST only)
  Maybe Text ->
  -- | timeout in milliseconds
  Int ->
  -- | (field name, dot-path) pairs to extract
  [(Text, Text)] ->
  -- | placeholder values
  Map.Map Text Text ->
  m ExecutionResult
executeIntegration method urlTemplate headerSpecs bodyTemplate timeoutMs fieldSpecs params = do
  let url = substituteTemplate params urlTemplate
  if "{{" `T.isInfixOf` url
    then pure $ failed Nothing 0 ("Unresolved placeholder in URL: " <> url)
    else do
      start <- liftIO getNowMs
      result <- liftIO $
        try @_ @SomeException $ do
          manager <-
            Http.newManager
              (HttpTLS.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeoutMs * 1000)})
          initReq <- Http.parseRequest (T.unpack url)
          let req =
                initReq
                  { Http.method = TE.encodeUtf8 method,
                    Http.requestHeaders =
                      [ (fromString (T.unpack k), TE.encodeUtf8 (substituteTemplate params v))
                        | (k, v) <- headerSpecs
                      ]
                        <> [(HttpTypes.hContentType, "application/json") | method == "POST"],
                    Http.requestBody = case (method, bodyTemplate) of
                      ("POST", Just body) -> Http.RequestBodyBS (TE.encodeUtf8 (substituteTemplate params body))
                      _ -> Http.RequestBodyBS ""
                  }
          Http.httpLbs req manager
      end <- liftIO getNowMs
      let latency = fromIntegral (end - start)
      case result of
        Left err -> do
          logError $ "ApiIntegrationExecutor: call failed for " <> url <> ": " <> show err
          pure $ failed Nothing latency (T.pack (displayException err))
        Right response -> do
          let status = HttpTypes.statusCode (Http.responseStatus response)
              bodyBytes = Http.responseBody response
              mbJson = A.decode @A.Value bodyBytes
          if status >= 200 && status < 300
            then case mbJson of
              Nothing ->
                pure $ failed (Just status) latency "Response was not valid JSON"
              Just json ->
                pure
                  ExecutionResult
                    { statusCode = Just status,
                      latencyMs = latency,
                      extractedFields = Map.fromList [(name, extractByPath fieldPath json) | (name, fieldPath) <- fieldSpecs],
                      rawResponse = Just json,
                      errorMessage = Nothing
                    }
            else
              pure $
                (failed (Just status) latency ("Non-success status: " <> show status))
                  { rawResponse = mbJson <|> Just (A.String (TE.decodeUtf8 (BSL.toStrict (BSL.take 2000 bodyBytes))))
                  }
  where
    failed status latency msg =
      ExecutionResult
        { statusCode = status,
          latencyMs = latency,
          extractedFields = Map.empty,
          rawResponse = Nothing,
          errorMessage = Just msg
        }

    getNowMs :: IO Integer
    getNowMs = round . (* 1000) . realToFrac @_ @Double <$> getPOSIXTime
