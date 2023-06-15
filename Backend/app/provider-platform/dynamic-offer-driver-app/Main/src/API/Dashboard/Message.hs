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
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Lib.API.Dashboard.Message as ADM
import qualified Lib.Domain.Action.Dashboard.Message as DMessage
import qualified Lib.Domain.Types.Message.Message as Domain
import qualified Lib.Domain.Types.Message.MessageReport as Domain
import Servant hiding (throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.Queries.Person as QP

type API =
  ADM.API

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  uploadFile merchantId
    :<|> addLinkAsMedia merchantId
    :<|> addMessage merchantId
    :<|> sendMessage merchantId
    :<|> messageList merchantId
    :<|> messageInfo merchantId
    :<|> messageDeliveryInfo merchantId
    :<|> messageReceiverList merchantId

sendMessageHandle :: ADM.ServiceHandle Flow
sendMessageHandle =
  ServiceHandle
    { findAllDriverIdExceptProvided = castFindAllDriverIdExceptProvided
    }

castFindAllDriverIdExceptProvided :: Transactionable m => Id Domain.Merchant -> [Id Domain.Driver] -> m [Id Domain.Driver]
castFindAllDriverIdExceptProvided mdMerchantId lsMdDriverId = map cast <$> QP.findAllDriverIdExceptProvided (cast mdMerchantId) (map cast lsMdDriverId)

addLinkAsMedia :: ShortId DM.Merchant -> Common.AddLinkAsMedia -> FlowHandler Common.UploadFileResponse
addLinkAsMedia merchantShortId = withFlowHandlerAPI . DMessage.addLinkAsMedia merchantShortId (findMerchantByShortId merchantShortId)

uploadFile :: ShortId DM.Merchant -> Common.UploadFileRequest -> FlowHandler Common.UploadFileResponse
uploadFile merchantShortId = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  DMessage.uploadFile merchantShortId merchant QTC.findByMerchantId merchant.id

addMessage :: ShortId DM.Merchant -> Common.AddMessageRequest -> FlowHandler Common.AddMessageResponse
addMessage merchantShortId = withFlowHandlerAPI . DMessage.addMessage merchantShortId (findMerchantByShortId merchantShortId)

sendMessage :: ShortId DM.Merchant -> Common.SendMessageRequest -> FlowHandler APISuccess
sendMessage merchantShortId = withFlowHandlerAPI . DMessage.sendMessage merchantShortId (findMerchantByShortId merchantShortId) sendMessageHandle

messageList :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> FlowHandler Common.MessageListResponse
messageList merchantShortId mbLimit =
  withFlowHandlerAPI . DMessage.messageList merchantShortId mbLimit (findMerchantByShortId merchantShortId)

messageInfo :: ShortId DM.Merchant -> Id Common.Message -> FlowHandler Common.MessageInfoResponse
messageInfo merchantShortId =
  withFlowHandlerAPI . DMessage.messageInfo merchantShortId . cast (findMerchantByShortId merchantShortId)

messageDeliveryInfo :: ShortId DM.Merchant -> Id Common.Message -> FlowHandler Common.MessageDeliveryInfoResponse
messageDeliveryInfo merchantShortId =
  withFlowHandlerAPI . DMessage.messageDeliveryInfo merchantShortId . cast (findMerchantByShortId merchantShortId)

messageReceiverList :: ShortId DM.Merchant -> Id Common.Message -> Maybe Text -> Maybe Common.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> FlowHandler Common.MessageReceiverListResponse
messageReceiverList merchantShortId messageId number status limit =
  withFlowHandlerAPI . DMessage.messageReceiverList merchantShortId (cast messageId) number status limit (findMerchantByShortId merchantShortId)
