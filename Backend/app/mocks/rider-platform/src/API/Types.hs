{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Types where

import Data.ByteString
import Kernel.Prelude
import Kernel.Types.App
import Kernel.Types.Beckn.Ack (AckResponse)
import qualified Kernel.Types.Beckn.Domain as Domain
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
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Capture "action" Text
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] AckResponse
