{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.Servant.Trail.Client where

import qualified Beckn.Storage.Queries.ExternalTrail as ExternalTrail
import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Storage.ExternalTrail as ExternalTrail
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified Beckn.Utils.Servant.Trail.Types as TT
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import Data.Binary.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Kind
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (EulerClient, JSONEx)
import Servant
import Servant.Client
import qualified Servant.Client.Core.Request as Client

-- | Request information which we ever want to record.
newtype RequestInfo = RequestInfo
  { _content :: TT.RequestContent
  }

-- | Internal type which makes client handlers remember info for tracing.
data ClientTracing verb

-- | Make client handlers return tracing info along with response data.
type family AddClientTracing (api :: Type) :: Type

type instance
  AddClientTracing (subApi :> api) =
    subApi :> AddClientTracing api

type instance
  AddClientTracing (api1 :<|> api2) =
    AddClientTracing api1 :<|> AddClientTracing api2

type instance
  AddClientTracing (Verb method status ctypes res) =
    ClientTracing (Verb method status ctypes res)

withClientTracing :: Proxy api -> Proxy (AddClientTracing api)
withClientTracing _ = Proxy

instance
  HasClient m verb =>
  HasClient m (ClientTracing verb)
  where
  type
    Client m (ClientTracing verb) =
      (RequestInfo, Client m verb)

  hoistClientMonad mp _ hst cli =
    hoistClientMonad mp (Proxy @verb) hst <$> cli

  clientWithRoute mp _ req =
    (toRequestInfo req,) $
      clientWithRoute mp (Proxy @verb) req
    where
      toRequestInfo :: Client.Request -> RequestInfo
      toRequestInfo request =
        RequestInfo
          { _content =
              TT.RequestContent
                { _path =
                    TT.decodePath $
                      LBS.toStrict . toLazyByteString $ Client.requestPath request,
                  _method = decodeUtf8 $ Client.requestMethod request,
                  _query =
                    toList $ TT.decodeQueryParam <$> Client.requestQueryString request,
                  _headers =
                    toList $ TT.decodeHeader <$> Client.requestHeaders request,
                  _body =
                    convertReqBody . fst
                      <$> Client.requestBody request
                }
          }

      convertReqBody :: Client.RequestBody -> LByteString
      convertReqBody = \case
        Client.RequestBodyLBS lbs -> lbs
        Client.RequestBodyBS bs -> LBS.fromStrict bs
        Client.RequestBodySource _ ->
          -- This is normally used for e.g. returning file contents,
          -- we don't want this to appear in trails.
          "<IO source>"

data TrailInfo
  = TrailInfo (Either ClientError LByteString) RequestInfo

saveClientTrailFlow :: TrailInfo -> FlowWithTraceFlag r ()
saveClientTrailFlow (TrailInfo res req) = do
  fork "save trail" do
    _id <- generateGUID
    dbResult <-
      ExternalTrail.create
        ExternalTrail.ExternalTrail
          { _gatewayId = "gw",
            ..
          }
    case dbResult of
      Left err -> do
        logTagError "client_trace" $
          "Failed to save request from gateway to " <> toText _endpointId <> ": " <> show err
      Right () -> pure ()
  where
    _endpointId = TT._endpointId $ _content req
    _queryParams = TT._queryString $ _content req
    _headers = TT._headersString $ _content req
    _request = decodeUtf8 <$> TT._body (_content req)
    _succeeded = Just $ isRight res
    _response = decodeUtf8 <$> rightToMaybe res
    _error = show <$> leftToMaybe res

-- TODO: merge with lib/beckn-core/src/Beckn/Utils/Error/Throwing.hs callCLient'
callAPIWithTrail' ::
  (JSONEx a, ToJSON a, HasCoreMetrics (FlowR r)) =>
  Maybe String ->
  BaseUrl ->
  (RequestInfo, EulerClient a) ->
  Text ->
  FlowWithTraceFlag r (Either ClientError a)
callAPIWithTrail' mbManager baseUrl (reqInfo, req) serviceName = do
  endTracking <- Metrics.startTracking (T.pack $ showBaseUrl baseUrl) serviceName
  res <- L.callAPI' mbManager baseUrl req
  let status = case res of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> T.pack $ show code
        Left (DecodeFailure _ (Response code _ _ _)) -> T.pack $ show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> T.pack $ show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> T.pack $ show code
        Left (ConnectionError _) -> "Connection error"
  _ <- endTracking status
  case res of
    Right r -> logTagInfo serviceName $ "Ok response: " <> decodeUtf8 (A.encode r)
    Left err -> logTagInfo serviceName $ "Error occured during client call: " <> show err
  traceFlag <- getTraceFlag
  case traceFlag of
    TRACE_OUTGOING -> trace res
    TRACE_ALL -> trace res
    _ -> return res
  where
    trace res' = do
      let trailInfo = TrailInfo (Aeson.encode <$> res') reqInfo
      saveClientTrailFlow trailInfo
      return res'

callAPIWithTrail ::
  (JSONEx a, ToJSON a, HasCoreMetrics (FlowR r)) =>
  BaseUrl ->
  (RequestInfo, EulerClient a) ->
  Text ->
  FlowWithTraceFlag r (Either ClientError a)
callAPIWithTrail = callAPIWithTrail' Nothing
