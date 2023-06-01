{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Message where

import qualified Domain.Action.UI.Message as DMessage
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Message.Message as Message
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "message"
    :> ( "list"
           :> TokenAuth
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] [DMessage.MessageAPIEntityResponse]
           :<|> Capture "messageId" (Id Message.Message)
             :> "seen"
             :> TokenAuth
             :> Put '[JSON] APISuccess
           :<|> Capture "messageId" (Id Message.Message)
             :> "like"
             :> TokenAuth
             :> Put '[JSON] APISuccess
           :<|> Capture "messageId" (Id Message.Message)
             :> "response"
             :> TokenAuth
             :> ReqBody '[JSON] DMessage.MessageReplyReq
             :> Put '[JSON] APISuccess
           :<|> "media" -- TODO : need to remove this apis once S3 is done.
             :> MandatoryQueryParam "filePath" Text
             :> TokenAuth
             :> Get '[JSON] Text
           :<|> Capture "messageId" (Id Message.Message)
             :> TokenAuth
             :> Get '[JSON] DMessage.MessageAPIEntityResponse
       )

handler :: FlowServer API
handler =
  messageList
    :<|> messageSeen
    :<|> messageLiked
    :<|> messageResponse
    :<|> fetchMedia
    :<|> getMessage

messageList :: (Id SP.Person, Id DM.Merchant) -> Maybe Int -> Maybe Int -> FlowHandler [DMessage.MessageAPIEntityResponse]
messageList (driverId, merchantId) mbLimit = withFlowHandlerAPI . DMessage.messageList (driverId, merchantId) mbLimit

getMessage :: Id Message.Message -> (Id SP.Person, Id DM.Merchant) -> FlowHandler DMessage.MessageAPIEntityResponse
getMessage msgId (driverId, merchantId) = withFlowHandlerAPI $ DMessage.getMessage (driverId, merchantId) msgId

messageSeen :: Id Message.Message -> (Id SP.Person, Id DM.Merchant) -> FlowHandler APISuccess
messageSeen msgId (driverId, merchantId) = withFlowHandlerAPI $ DMessage.messageSeen (driverId, merchantId) msgId

messageLiked :: Id Message.Message -> (Id SP.Person, Id DM.Merchant) -> FlowHandler APISuccess
messageLiked msgId (driverId, merchantId) = withFlowHandlerAPI $ DMessage.messageLiked (driverId, merchantId) msgId

messageResponse :: Id Message.Message -> (Id SP.Person, Id DM.Merchant) -> DMessage.MessageReplyReq -> FlowHandler APISuccess
messageResponse msgId (driverId, merchantId) = withFlowHandlerAPI . DMessage.messageResponse (driverId, merchantId) msgId

fetchMedia :: Text -> (Id SP.Person, Id DM.Merchant) -> FlowHandler Text
fetchMedia filePath (driverId, merchantId) = withFlowHandlerAPI $ DMessage.fetchMedia (driverId, merchantId) filePath
