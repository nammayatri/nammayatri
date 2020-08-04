{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
-- See [request-body-substitution] note below
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Saving information about requests.
module Beckn.Utils.Servant.Trail.Server where

import qualified Beckn.Utils.Servant.Trail.Types as T
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as LBS
import EulerHS.Prelude
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Network
import qualified Network.Wai as Wai

-- | Request information which we ever want to record.
--
-- We introduce this datatype because 'Wai.Request' changes during
-- processing of a call to an endpoint, we don't want to use it in
-- the interface of this module.
data RequestInfo = RequestInfo
  { -- | Most of the request content.
    _content :: T.RequestContent,
    -- | Peer address.
    _remoteHost :: Network.SockAddr,
    -- | Whether SSL connection is used.
    _isSecure :: Bool
  }
  deriving (Show)

lookupRequestHeader :: Text -> RequestInfo -> Maybe Text
lookupRequestHeader name RequestInfo {..} =
  snd <$> find ((== name) . fst) (T._headers _content)

-- | Read information about request.
--
-- This depletes the original request body stream, you have to use
-- the returned request instead.
toRequestInfo :: Wai.Request -> IO (RequestInfo, Wai.Request)
toRequestInfo req = do
  (body, bodyStream) <- forkBytesStream (Wai.getRequestBodyChunk req)
  let reqInfo =
        RequestInfo
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

-- | Response information which we ever want to record.
data ResponseInfo = ResponseInfo
  { _statusCode :: Int,
    _statusMessage :: Text,
    _responseSucceeded :: Bool,
    _responseBody :: LByteString,
    _responseHeaders :: [(Text, Text)]
  }

-- | Status code and message put in one string.
_responseStatus :: ResponseInfo -> LText
_responseStatus ResponseInfo {..} =
  show _statusCode <> " / " <> toLText _statusMessage

-- | All headers put into one string.
_responseHeadersString :: ResponseInfo -> LText
_responseHeadersString = T.keyValueToString . _responseHeaders

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
  { _preAction :: RequestInfo -> m v,
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
