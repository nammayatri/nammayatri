{-# LANGUAGE TypeApplications #-}

module Utils.Common where

import App.Types
import qualified Beckn.Storage.Queries.ExternalTrail as TQ
import Beckn.Types.Common
import qualified Beckn.Types.Storage.ExternalTrail as TS
import Beckn.Utils.Common (encodeToText')
import Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified Beckn.Utils.Servant.Trail.Client as UT
import qualified Beckn.Utils.Servant.Trail.Types as UT
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import EulerHS.Types (EulerClient, JSONEx)
import Servant.Client.Core (BaseUrl)
import Servant.Client.Core.ClientError
import Servant.Client.Core.Response

callAPI ::
  (JSONEx a, ToJSON a) =>
  BaseUrl ->
  (UT.RequestInfo, EulerClient a) ->
  Text ->
  Flow (Either ClientError a)
callAPI baseUrl (reqInfo, req) serviceName = do
  endTracking <- L.runUntracedIO $ Metrics.startTracking (encodeToText' baseUrl) serviceName
  res <- L.callAPI baseUrl req
  let trailInfo = TrailInfo (Aeson.encode <$> res) reqInfo
  let status = case res of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> T.pack $ show code
        Left (DecodeFailure _ (Response code _ _ _)) -> T.pack $ show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> T.pack $ show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> T.pack $ show code
        Left (ConnectionError _) -> "Connection error"
  _ <- L.runUntracedIO $ endTracking status
  _ <- saveClientTrailFlow trailInfo
  return res

data TrailInfo
  = TrailInfo (Either ClientError LByteString) UT.RequestInfo

saveClientTrailFlow :: TrailInfo -> Flow ()
saveClientTrailFlow (TrailInfo res req) = do
  fork "save trail" do
    _id <- generateGUID
    dbResult <-
      TQ.create
        TS.ExternalTrail
          { _gatewayId = "gw",
            ..
          }
    case dbResult of
      Left err -> do
        L.logError @Text "client_trace" $
          "Failed to save request from gateway to " <> toText _endpointId <> show err
      Right () -> pure ()
  pure ()
  where
    _endpointId = UT._endpointId $ UT._content req
    _queryParams = UT._queryString $ UT._content req
    _headers = UT._headersString $ UT._content req
    _request = decodeUtf8 <$> UT._body (UT._content req)
    _succeeded = Just $ isRight res
    _response = decodeUtf8 <$> rightToMaybe res
    _error = show <$> leftToMaybe res

fork :: Text -> Flow () -> Flow ()
fork desc f = do
  env <- ask
  lift $ L.forkFlow desc $ runReaderT f env
