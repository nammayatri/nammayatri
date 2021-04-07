{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
-- See [request-body-substitution] note below
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Saving information about requests.
module Beckn.Utils.Servant.Trail.Server where

import Beckn.Storage.DB.Config (HasDbCfg)
import qualified Beckn.Storage.Queries.Trail as Trail
import Beckn.Types.App
import Beckn.Types.Common hiding (id)
import qualified Beckn.Types.Storage.Trail as Trail
import Beckn.Utils.Common
import Beckn.Utils.Servant.Trail.Types
import qualified Beckn.Utils.Servant.Trail.Types as T
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Time (UTCTime, diffUTCTime)
import EulerHS.Prelude
import GHC.Records (HasField (..))
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

-- | Read information about request.
--
-- This depletes the original request body stream, you have to use
-- the returned request instead.
toRequestInfo :: Wai.Request -> IO (ServerRequestInfo, Wai.Request)
toRequestInfo req = do
  (body, bodyStream) <- forkBytesStream (Wai.getRequestBodyChunk req)
  let reqInfo =
        ServerRequestInfo
          { _remoteHost = Wai.remoteHost req,
            _isSecure = Wai.isSecure req,
            _content =
              T.RequestContent
                { _method = decodeUtf8 $ Wai.requestMethod req,
                  _query = T.decodeQueryParam <$> Wai.queryString req,
                  _headers = T.decodeHeader <$> Wai.requestHeaders req,
                  _path =
                    -- Cannot use 'Wai.pathInfo' directly because it is empty at this point
                    T.decodePath $ Wai.rawPathInfo req,
                  _body = body <$ guard (not (null body))
                }
          }
      -- [Note: request-body-substitution]
      -- We insolently delve into Wai internals when reading request body
      -- strictly, and here we have to substitute field with body stream.
      -- 'Wai.requestBody' was deprecated, and now proposes 'Wai.getRequestBodyChunk'
      -- as an alternative getter. Though we want to modify the field, so have
      -- to refer to the original 'Wai.requestBody' field.
      req' = req {Wai.requestBody = bodyStream}
  return (reqInfo, req')

-- | For given bytes stream represented as @IO ByteString@, read it
-- entirely and return its content, and also return a stream semantically equal
-- to the given one.
forkBytesStream :: IO ByteString -> IO (LByteString, IO ByteString)
forkBytesStream doRead = do
  contents <- readAll
  var <- newIORef contents
  let consumeVar =
        atomicModifyIORef' var $ \case
          [] -> ([], mempty)
          (c : cs) -> (cs, c)
  return (LBS.fromChunks contents, consumeVar)
  where
    readAll =
      doRead >>= \case
        "" -> return []
        chunk -> (chunk :) <$> readAll

-- | Read response info from raw 'Wai.Response'.
--
-- Note that this will read the entire response body, may be not what you want
-- if your API is able of returning a file, for instance.
toResponseInfo :: Wai.Response -> IO ResponseInfo
toResponseInfo resp = do
  let (status, headers, bodyWriter) = Wai.responseToStream resp
  body <- bodyWriter bodyToBytestring
  let statusCode = HTTP.statusCode status
  return
    ResponseInfo
      { _statusCode = statusCode,
        _statusMessage = decodeUtf8 $ HTTP.statusMessage status,
        _responseSucceeded = statusCode >= 200 && statusCode < 300,
        _responseBody = body,
        _responseHeaders = T.decodeHeader <$> headers
      }
  where
    bodyToBytestring :: Wai.StreamingBody -> IO LByteString
    bodyToBytestring streamingBody = do
      content <- newIORef mempty
      streamingBody (\chunk -> modifyIORef' content (<> chunk)) pass
      -- Someday can do lazy IO above to spare some memory allocations
      B.toLazyByteString <$> readIORef content

-- | Handlers to be invoked on each request.
--
-- Actions provided here have to be no-throw.
data TraceHandler m = forall v.
  TraceHandler
  { _preAction :: ServerRequestInfo -> m v,
    _postAction :: v -> ResponseInfo -> m ()
  }

hoistTraceHandler :: (forall a. m a -> n a) -> TraceHandler m -> TraceHandler n
hoistTraceHandler hst TraceHandler {..} =
  TraceHandler
    { _preAction = hst . _preAction,
      _postAction = hst ... _postAction
    }

-- | Enable tracing of incoming requests and responses using the provided
-- handlers.
--
-- Note that this kills all the laziness used for request and response bodies,
-- so may increase taken time and memory consumption.
-- addServantInfo proxy app request respond =
traceRequests :: TraceHandler IO -> Wai.Middleware
traceRequests TraceHandler {..} app request respond =
  do
    (reqInfo, request') <- toRequestInfo request
    v <- _preAction reqInfo
    app request' $
      \response ->
        do
          responded <- respond response
          respInfo <- toResponseInfo response
          _postAction v respInfo
          return responded

-- TODO: add a test on that request arguments appear in database at least
-- for one entrypoint

mkTrail :: Text -> UTCTime -> ServerRequestInfo -> Trail.Trail
mkTrail reqId now req =
  Trail.Trail
    { _id = reqId,
      --_customerId = CustomerId <$> lookupRequestHeader "customerId" req,
      --_sessionId = SessionId <$> lookupRequestHeader "sessionId" req,
      _endpointId = _endpointId $ _content req,
      _headers = _headersString $ _content req,
      _queryParams = _queryString $ _content req,
      _requestBody = decodeUtf8 <$> _body (_content req),
      _remoteHost = show $ _remoteHost req,
      _isSecure = _isSecure req,
      _succeeded = Nothing,
      _responseBody = Nothing,
      _responseStatus = Nothing,
      _responseHeaders = Nothing,
      _createdAt = now,
      _processDuration = Nothing
    }

traceHandler :: HasDbCfg r => TraceHandler (FlowR r)
traceHandler = TraceHandler {..}
  where
    _preAction req = do
      reqId <- generateGUID
      now <- getCurrentTime
      let endpointId = toString $ _endpointId $ _content req
      if "v1 GET" /= endpointId && "v1/ GET" /= endpointId
        then do
          let trail = mkTrail reqId now req
          pure (Just (reqId, now, trail))
        else pure Nothing
    _postAction Nothing _ = pass
    _postAction (Just (reqId, reqTime, trail)) res = do
      now <- getCurrentTime
      let duration = roundDiffTimeToUnit $ now `diffUTCTime` reqTime
      fork "save trail" $ do
        Trail.create trail >>= \case
          Left err -> do
            logTagError "trace" $
              "Saving request failed: " <> show err
          Right () -> pass
        dbres <- Trail.setResponseInfo reqId duration res
        case dbres of
          Left err ->
            logTagError "trace" $
              "Saving response on " <> show reqId <> " failed: " <> show err
          Right () -> pass
      pass

traceHandler' :: HasDbCfg r => EnvR r -> TraceHandler IO
traceHandler' env =
  hoistTraceHandler (runFlowR (runTime env) (appEnv env)) traceHandler

toTraceOrNotToTrace ::
  (HasTraceFlag r, HasDbCfg r) =>
  EnvR r ->
  Wai.Application ->
  Wai.Application
toTraceOrNotToTrace env app = case traceFlag of
  TRACE_OUTGOING -> app
  TRACE_NOTHING -> app
  _ -> traceRequests (traceHandler' env) app
  where
    traceFlag = getField @"traceFlag" (appEnv env)
