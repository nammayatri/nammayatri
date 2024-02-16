{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Common (sendDLVerification, sendRCVerification, buildMeaninglessIdfyResponse) where

import App.Types
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Kernel.External.Verification.Interface.Idfy
import Kernel.Utils.Common hiding (Error)
import Types.Common

sendVerification :: (ToJSON a) => Text -> a -> Flow AckResponse
sendVerification tag req = do
  url <- asks (.webhookUrl)
  secret <- asks (.secret)
  callWebhookAPI url (task secret) tag (Proxy @IdfyWebhookAPI)
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

callWebhookAPI :: CallAPI env api res
callWebhookAPI = callApiUnwrappingApiError (identity @Error) Nothing Nothing Nothing

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
