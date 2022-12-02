module API.Types where

import Beckn.Prelude
import Beckn.Types.App
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Utils.Servant.JSONBS
import Beckn.Utils.Servant.SignatureAuth
import Data.ByteString
import Servant

type API =
  TriggerAPI
    :<|> CallbackReceiverAPI

type TriggerAPI =
  "trigger"
    :> MandatoryQueryParam "url_to_trigger" Text
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] AckResponse

type CallbackReceiverAPI =
  "callback_receiver"
    :> SignatureAuth "Authorization"
    :> Capture "action" Text
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] AckResponse
