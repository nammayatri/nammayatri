module Common (sendDLVerification, sendRCVerification) where

import App.Types
import Beckn.Utils.Common hiding (Error)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Servant (Header, JSON, Post, ReqBody, (:>))
import Types.Common
import Types.Webhook

type DLWebhookAPI =
  "ui" :> "ext" :> "idfy" :> "drivingLicense"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] DLVerificationResponse
    :> Post '[JSON] AckResponse

dlWebhookAPI :: Proxy DLWebhookAPI
dlWebhookAPI = Proxy

type RCWebhookAPI =
  "ui" :> "ext" :> "idfy" :> "vehicleRegistrationCert"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] RCVerificationResponse
    :> Post '[JSON] AckResponse

rcWebhookAPI :: Proxy RCWebhookAPI
rcWebhookAPI = Proxy

sendDLVerification :: DLVerificationResponse -> Flow AckResponse
sendDLVerification req = do
  url <- asks (.webhookUrl)
  secret <- asks (.secret)
  callWebhookAPI url (task secret) "DLVerificationResult"
  where
    task secret =
      T.client
        dlWebhookAPI
        (Just secret)
        req

sendRCVerification :: RCVerificationResponse -> Flow AckResponse
sendRCVerification req = do
  url <- asks (.webhookUrl)
  secret <- asks (.secret)
  callWebhookAPI url (task secret) "RCVerificationResult"
  where
    task secret =
      T.client
        rcWebhookAPI
        (Just secret)
        req

callWebhookAPI :: CallAPI env res
callWebhookAPI = callApiUnwrappingApiError (identity @Error) Nothing Nothing