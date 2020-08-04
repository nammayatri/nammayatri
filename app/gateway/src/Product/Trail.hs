{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Product.Trail where

import App.Types
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Trail as Trail
import Beckn.Utils.Common
import qualified Beckn.Utils.Servant.Trail.Server as Util
import qualified Beckn.Utils.Servant.Trail.Types as Util
import Data.Time
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Trail as Trail

-- TODO: add a test on that request arguments appear in database at least
-- for one entrypoint

mkTrail :: Text -> LocalTime -> Util.RequestInfo -> Trail.Trail
mkTrail reqId now req =
  Trail.Trail
    { _id = reqId,
      --_customerId = CustomerId <$> Util.lookupRequestHeader "customerId" req,
      --_sessionId = SessionId <$> Util.lookupRequestHeader "sessionId" req,
      _endpointId = Util._endpointId $ Util._content req,
      _headers = Util._headersString $ Util._content req,
      _queryParams = Util._queryString $ Util._content req,
      _requestBody = decodeUtf8 <$> Util._body (Util._content req),
      _remoteHost = show $ Util._remoteHost req,
      _isSecure = Util._isSecure req,
      _succeeded = Nothing,
      _responseBody = Nothing,
      _responseStatus = Nothing,
      _responseHeaders = Nothing,
      _createdAt = now,
      _processDuration = Nothing
    }

traceHandler :: Util.TraceHandler Flow
traceHandler = Util.TraceHandler {..}
  where
    _preAction req = do
      reqId <- generateGUID
      now <- getCurrTime
      Trail.create (mkTrail reqId now req) >>= \case
        Left err -> do
          L.logError @Text "trace" $
            "Saving request failed: " <> show err
          return Nothing
        Right () -> pure (Just (reqId, now))
    _postAction Nothing _ = pass
    _postAction (Just (reqId, reqTime)) res = do
      now <- getCurrTime
      let duration = roundDiffTimeToUnit $ now `diffLocalTime` reqTime
      Trail.setResponseInfo reqId duration res >>= \case
        Left err ->
          L.logError @Text "trace" $
            "Saving response on " <> show reqId <> " failed: " <> show err
        Right () -> pass
