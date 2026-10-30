module SharedLogic.External.Umis.Flow (getStudentDetails) where

import qualified EulerHS.Language as L
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common
import Network.HTTP.Types.Status (statusCode)
import Servant.Client.Core (ClientError (..), ResponseF (..))
import qualified SharedLogic.External.Umis.API as UmisAPI
import SharedLogic.External.Umis.Types
import Tools.Error (PassError (UmisLookupFailed))

-- status code 204 for empty body.
getStudentDetails :: (Metrics.CoreMetrics m, MonadFlow m) => UmisConfig -> Text -> m (Maybe UmisStudentResp)
getStudentDetails cfg umisId = do
  res <-
    measuringDuration (Metrics.addRequestLatency (showBaseUrl cfg.url) "umisStudentDetails") $
      L.callAPI' (Just defaultHttpManager) cfg.url (UmisAPI.getUmisStudentDetails umisId (Just cfg.authToken))
  case res of
    Right student -> pure (Just student)
    Left err -> case errorStatus err of
      Just 204 -> pure Nothing
      Just 401 -> do
        logError "UMIS rejected the configured auth token (401)"
        throwError $ InternalError "UMIS authentication failed"
      mbStatus -> do
        logError $ "UMIS student lookup failed, status: " <> maybe "no response" show mbStatus
        throwError UmisLookupFailed
  where
    errorStatus :: ClientError -> Maybe Int
    errorStatus = \case
      FailureResponse _ resp -> Just (statusOf resp)
      DecodeFailure _ resp -> Just (statusOf resp)
      UnsupportedContentType _ resp -> Just (statusOf resp)
      InvalidContentTypeHeader resp -> Just (statusOf resp)
      ConnectionError _ -> Nothing
    statusOf = statusCode . responseStatusCode
