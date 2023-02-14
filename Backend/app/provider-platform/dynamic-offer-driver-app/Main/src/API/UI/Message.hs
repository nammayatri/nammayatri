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
             :> "response"
             :> TokenAuth
             :> ReqBody '[JSON] DMessage.MessageReplyReq
             :> Put '[JSON] APISuccess
           :<|> "media" -- TODO : need to remove this apis once S3 is done.
             :> MandatoryQueryParam "filePath" Text
             :> TokenAuth
             :> Get '[JSON] Text
       )

handler :: FlowServer API
handler =
  messageList
    :<|> messageSeen
    :<|> messageResponse
    :<|> fetchMedia

messageList :: Id SP.Person -> Maybe Int -> Maybe Int -> FlowHandler [DMessage.MessageAPIEntityResponse]
messageList driverId mbLimit = withFlowHandlerAPI . DMessage.messageList driverId mbLimit

messageSeen :: Id Message.Message -> Id SP.Person -> FlowHandler APISuccess
messageSeen msgId driverId = withFlowHandlerAPI $ DMessage.messageSeen driverId msgId

messageResponse :: Id Message.Message -> Id SP.Person -> DMessage.MessageReplyReq -> FlowHandler APISuccess
messageResponse msgId driverId = withFlowHandlerAPI . DMessage.messageResponse driverId msgId

fetchMedia :: Text -> Id SP.Person -> FlowHandler Text
fetchMedia filePath driverId = withFlowHandlerAPI $ DMessage.fetchMedia driverId filePath
