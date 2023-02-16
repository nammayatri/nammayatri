module API.Dashboard.Message where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified Domain.Action.Dashboard.Message as DMessage
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (throwError)

type API =
  "message"
    :> ( Common.UploadFileAPI
           :<|> Common.AddMessageAPI
           :<|> Common.SendMessageAPI
           :<|> Common.MessageListAPI
           :<|> Common.MessageInfoAPI
           :<|> Common.MessageDeliveryInfoAPI
           :<|> Common.MessageReceiverListAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  uploadFile merchantId
    :<|> addMessage merchantId
    :<|> sendMessage merchantId
    :<|> messageList merchantId
    :<|> messageInfo merchantId
    :<|> messageDeliveryInfo merchantId
    :<|> messageReceiverList merchantId

uploadFile :: ShortId DM.Merchant -> Common.UploadFileRequest -> FlowHandler Common.UploadFileResponse
uploadFile merchantShortId = withFlowHandlerAPI . DMessage.uploadFile merchantShortId

addMessage :: ShortId DM.Merchant -> Common.AddMessageRequest -> FlowHandler Common.AddMessageResponse
addMessage merchantShortId = withFlowHandlerAPI . DMessage.addMessage merchantShortId

sendMessage :: ShortId DM.Merchant -> Common.SendMessageRequest -> FlowHandler APISuccess
sendMessage merchantShortId = withFlowHandlerAPI . DMessage.sendMessage merchantShortId

messageList :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> FlowHandler Common.MessageListResponse
messageList merchantShortId mbLimit =
  withFlowHandlerAPI . DMessage.messageList merchantShortId mbLimit

messageInfo :: ShortId DM.Merchant -> Id Common.Message -> FlowHandler Common.MessageInfoResponse
messageInfo merchantShortId =
  withFlowHandlerAPI . DMessage.messageInfo merchantShortId . cast

messageDeliveryInfo :: ShortId DM.Merchant -> Id Common.Message -> FlowHandler Common.MessageDeliveryInfoResponse
messageDeliveryInfo merchantShortId =
  withFlowHandlerAPI . DMessage.messageDeliveryInfo merchantShortId . cast

messageReceiverList :: ShortId DM.Merchant -> Id Common.Message -> Maybe Text -> Maybe Common.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> FlowHandler Common.MessageReceiverListResponse
messageReceiverList merchantShortId messageId number status limit =
  withFlowHandlerAPI . DMessage.messageReceiverList merchantShortId (cast messageId) number status limit
