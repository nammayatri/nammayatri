{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Message where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified Domain.Action.Dashboard.Message as DMessage
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (throwError)

type API =
  "message"
    :> ( Common.UploadFileAPI
           :<|> Common.AddLinkAPI
           :<|> Common.AddMessageAPI
           :<|> Common.SendMessageAPI
           :<|> Common.MessageListAPI
           :<|> Common.MessageInfoAPI
           :<|> Common.MessageDeliveryInfoAPI
           :<|> Common.MessageReceiverListAPI
       )

handler :: ShortId DM.Merchant -> City.City -> FlowServer API
handler merchantId city =
  uploadFile merchantId city
    :<|> addLinkAsMedia merchantId city
    :<|> addMessage merchantId city
    :<|> sendMessage merchantId city
    :<|> messageList merchantId city
    :<|> messageInfo merchantId city
    :<|> messageDeliveryInfo merchantId city
    :<|> messageReceiverList merchantId city

addLinkAsMedia :: ShortId DM.Merchant -> City.City -> Common.AddLinkAsMedia -> FlowHandler Common.UploadFileResponse
addLinkAsMedia merchantShortId opCity = withFlowHandlerAPI . DMessage.addLinkAsMedia merchantShortId opCity

uploadFile :: ShortId DM.Merchant -> City.City -> Common.UploadFileRequest -> FlowHandler Common.UploadFileResponse
uploadFile merchantShortId opCity = withFlowHandlerAPI . DMessage.uploadFile merchantShortId opCity

addMessage :: ShortId DM.Merchant -> City.City -> Common.AddMessageRequest -> FlowHandler Common.AddMessageResponse
addMessage merchantShortId opCity = withFlowHandlerAPI . DMessage.addMessage merchantShortId opCity

sendMessage :: ShortId DM.Merchant -> City.City -> Common.SendMessageRequest -> FlowHandler APISuccess
sendMessage merchantShortId opCity = withFlowHandlerAPI . DMessage.sendMessage merchantShortId opCity

messageList :: ShortId DM.Merchant -> City.City -> Maybe Int -> Maybe Int -> FlowHandler Common.MessageListResponse
messageList merchantShortId opCity mbLimit =
  withFlowHandlerAPI . DMessage.messageList merchantShortId opCity mbLimit

messageInfo :: ShortId DM.Merchant -> City.City -> Id Common.Message -> FlowHandler Common.MessageInfoResponse
messageInfo merchantShortId opCity =
  withFlowHandlerAPI . DMessage.messageInfo merchantShortId opCity . cast

messageDeliveryInfo :: ShortId DM.Merchant -> City.City -> Id Common.Message -> FlowHandler Common.MessageDeliveryInfoResponse
messageDeliveryInfo merchantShortId opCity =
  withFlowHandlerAPI . DMessage.messageDeliveryInfo merchantShortId opCity . cast

messageReceiverList :: ShortId DM.Merchant -> City.City -> Id Common.Message -> Maybe Text -> Maybe Common.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> FlowHandler Common.MessageReceiverListResponse
messageReceiverList merchantShortId opCity messageId number status limit =
  withFlowHandlerAPI . DMessage.messageReceiverList merchantShortId opCity (cast messageId) number status limit
