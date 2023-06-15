{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Message where

import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Lib.API.UI.Message as MSG
import qualified Lib.Domain.Action.UI.Message as DMessage
import qualified Lib.Domain.Types.Message.Message as Message
import Servant
import qualified Storage.Queries.Person as Person
import Tools.Auth

type API =
  TokenAuth
    :> MSG.CommonMessageAPI

handler :: FlowServer API
handler = a
  where
    a pid =
      messageList pid
        :<|> messageSeen pid
        :<|> messageLiked pid
        :<|> messageResponse pid
        :<|> fetchMedia pid
        :<|> getMessage pid

messageList :: Id SP.Person -> Maybe Int -> Maybe Int -> FlowHandler [DMessage.MessageAPIEntityResponse]
messageList driverId mbLimit mbOffset = withFlowHandlerAPI $ DMessage.messageList (cast driverId) mbLimit mbOffset (Person.findById driverId)

messageSeen :: Id SP.Person -> Id Message.Message -> FlowHandler APISuccess
messageSeen driverId msgId = withFlowHandlerAPI $ DMessage.messageSeen (cast driverId) msgId (Person.findById driverId)

messageLiked :: Id SP.Person -> Id Message.Message -> FlowHandler APISuccess
messageLiked driverId msgId = withFlowHandlerAPI $ DMessage.messageLiked (cast driverId) msgId (Person.findById driverId)

messageResponse :: Id SP.Person -> Id Message.Message -> DMessage.MessageReplyReq -> FlowHandler APISuccess
messageResponse driverId msgId messageReplyReq = withFlowHandlerAPI $ DMessage.messageResponse (cast driverId) msgId messageReplyReq (Person.findById driverId)

fetchMedia :: Id SP.Person -> Text -> FlowHandler Text
fetchMedia driverId filePath = withFlowHandlerAPI $ DMessage.fetchMedia (cast driverId) filePath (Person.findById driverId)

getMessage :: Id SP.Person -> Id Message.Message -> FlowHandler DMessage.MessageAPIEntityResponse
getMessage driverId msgId = withFlowHandlerAPI $ DMessage.getMessage (cast driverId) msgId (Person.findById driverId)
