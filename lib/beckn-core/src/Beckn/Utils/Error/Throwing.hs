module Beckn.Utils.Error.Throwing where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Error.API
import Beckn.Types.Error.APIError
import Beckn.Utils.Logging
import Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import qualified Servant.Client as S
import Servant.Client.Core

type IsAPIException e = (IsAPIError e, Exception e)

throwError :: (MonadThrow m, Log m, IsAPIException e) => e -> m b
throwError err = do
  logWarning $ toLogMessageAPIError err
  throwM err

fromMaybeM ::
  (MonadThrow m, Log m, IsAPIException e) => e -> Maybe b -> m b
fromMaybeM err = maybe (throwError err) pure

fromEitherM ::
  (MonadThrow m, Log m, IsAPIException e) => (left -> e) -> Either left b -> m b
fromEitherM toerr = either (throwError . toerr) pure

throwDBError :: (MonadThrow m, Log m) => ET.DBError -> m a
throwDBError (ET.DBError dbErrType msg) = throwError $
  case dbErrType of
    ET.UnexpectedResult -> SQLResultError msg
    ET.SQLError sqlErr -> SQLRequestError (show sqlErr) msg
    _ -> DBUnknownError msg

checkDBError :: (MonadThrow m, Log m) => ET.DBResult a -> m a
checkDBError = either throwDBError pure

checkDBErrorOrEmpty ::
  (MonadThrow m, Log m, IsAPIException b) =>
  ET.DBResult (Maybe a) ->
  b ->
  m a
checkDBErrorOrEmpty dbres domainErrOnEmpty =
  either throwDBError (fromMaybeM domainErrOnEmpty) dbres

-- TODO: move these functions somewhere else:
callClient ::
  (ET.JSONEx a, L.MonadFlow m, Log m) =>
  Text ->
  Context ->
  S.BaseUrl ->
  ET.EulerClient a ->
  m a
callClient = callClient' Nothing

-- TODO: the @desc@ argument should become part of monadic context
-- TODO: merge with lib/beckn-core/src/Beckn/Utils/Servant/Trail/Client.hs callAPIWithTrail'
callClient' ::
  (ET.JSONEx a, L.MonadFlow m, Log m) =>
  Maybe String ->
  Text ->
  context ->
  S.BaseUrl ->
  ET.EulerClient a ->
  m a
callClient' mbManager desc _ baseUrl cli = do
  endTracking <- L.runIO $ Metrics.startTracking (T.pack $ showBaseUrl baseUrl) desc
  res <- L.callAPI' mbManager baseUrl cli
  _ <- L.runIO $ endTracking $ getResponseCode res
  res & fromEitherM (ExternalAPICallError baseUrl)
  where
    getResponseCode res =
      case res of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> T.pack $ show code
        Left (DecodeFailure _ (Response code _ _ _)) -> T.pack $ show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> T.pack $ show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> T.pack $ show code
        Left (ConnectionError _) -> "Connection error"
