module API.Types where

import Data.ByteString
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Utils.Servant.JSONBS
import Kernel.Utils.Servant.SignatureAuth
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
