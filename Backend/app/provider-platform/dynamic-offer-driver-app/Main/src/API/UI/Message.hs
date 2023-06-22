{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Message where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, withFlowHandlerAPI)
import qualified Lib.API.Message as MSG
import qualified Lib.Domain.Action.UI.Message as DMessage
import qualified Lib.Domain.Types.Message.Message as Message
import Servant
import qualified Storage.Queries.Person as Person
import Tools.Auth

type API =
  TokenAuth
    :> MSG.UiAPI

handler :: FlowServer API
handler = wrapperHandler
  where
    wrapperHandler (pid, mid) =
      messageList (pid, mid)
        :<|> messageSeen (pid, mid)
        :<|> messageLiked (pid, mid)
        :<|> messageResponse (pid, mid)
        :<|> fetchMedia (pid, mid)
        :<|> getMessage (pid, mid)

messageList :: (Id SP.Person, Id DM.Merchant) -> Maybe Int -> Maybe Int -> FlowHandler [DMessage.MessageAPIEntityResponse]
messageList (driverId, merchantId) mbLimit mbOffset = withFlowHandlerAPI $ do
  person <- Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  DMessage.messageList (cast driverId, cast merchantId) mbLimit mbOffset person.language

getMessage :: (Id SP.Person, Id DM.Merchant) -> Id Message.Message -> FlowHandler DMessage.MessageAPIEntityResponse
getMessage (driverId, merchantId) msgId = withFlowHandlerAPI $ do
  person <- Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  DMessage.getMessage (cast driverId, cast merchantId) msgId person.language

messageSeen :: (Id SP.Person, Id DM.Merchant) -> Id Message.Message -> FlowHandler APISuccess
messageSeen (driverId, merchantId) msgId = withFlowHandlerAPI $ DMessage.messageSeen (cast driverId, cast merchantId) msgId

messageLiked :: (Id SP.Person, Id DM.Merchant) -> Id Message.Message -> FlowHandler APISuccess
messageLiked (driverId, merchantId) msgId = withFlowHandlerAPI $ do DMessage.messageLiked (cast driverId, cast merchantId) msgId

messageResponse :: (Id SP.Person, Id DM.Merchant) -> Id Message.Message -> DMessage.MessageReplyReq -> FlowHandler APISuccess
messageResponse (driverId, merchantId) msgId messageReplyReq = withFlowHandlerAPI $ DMessage.messageResponse (cast driverId, cast merchantId) msgId messageReplyReq

fetchMedia :: (Id SP.Person, Id DM.Merchant) -> Text -> FlowHandler Text
fetchMedia (driverId, merchantId) filePath = withFlowHandlerAPI $ DMessage.fetchMedia (cast driverId, cast merchantId) filePath
