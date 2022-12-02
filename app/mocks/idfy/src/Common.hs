module Common (sendDLVerification, sendRCVerification, buildMeaninglessIdfyResponse) where

import App.Types
import Beckn.Utils.Common hiding (Error)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Idfy.Flow
import Idfy.Types.Response
import Types.Common

sendVerification :: (ToJSON a) => Text -> a -> Flow AckResponse
sendVerification tag req = do
  url <- asks (.webhookUrl)
  secret <- asks (.secret)
  callWebhookAPI url (task secret) tag
  where
    task secret =
      T.client
        (Proxy @IdfyWebhookAPI)
        (Just secret)
        (toJSON req)

sendDLVerification :: VerificationResponse -> Flow AckResponse
sendDLVerification = sendVerification "DLVerificationResult"

sendRCVerification :: VerificationResponse -> Flow AckResponse
sendRCVerification = sendVerification "RCVerificationResult"

callWebhookAPI :: CallAPI env res
callWebhookAPI = callApiUnwrappingApiError (identity @Error) Nothing Nothing

buildMeaninglessIdfyResponse :: (MonadTime m, MonadGuid m) => Maybe a -> m (IdfyResponse a)
buildMeaninglessIdfyResponse mbRes = do
  reqId <- generateGUID
  now <- getCurrentTime
  pure
    IdfyResponse
      { action = "action",
        completed_at = now,
        created_at = now,
        group_id = "gid",
        request_id = reqId,
        result = mbRes,
        status = "status",
        task_id = "task_id",
        _type = "type"
      }
