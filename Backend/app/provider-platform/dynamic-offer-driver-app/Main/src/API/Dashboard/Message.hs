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
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import qualified Lib.API.Message as ADM
import qualified Lib.Domain.Action.Dashboard.Message as DMessage
import qualified Lib.Domain.Types.Message.Message as Domain
import qualified Lib.Domain.Types.Message.MessageReport as Domain
import Servant hiding (throwError)
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.CachedQueries.Merchant.TransporterConfig as QTC
import Storage.Queries.Person (findAllDriverIdExceptProvided, findDriverByIdsIn)

type API =
  ADM.DashboardAPI

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

sendMessageHandle :: DMessage.ServiceHandle Flow
sendMessageHandle =
  DMessage.ServiceHandle
    { findAllPersonIdExceptProvided = castFindAllDriverIdExceptProvided,
      findPersonInIds = castFindPersonInIds
    }

castFindPersonInIds :: (Transactionable m, Monad m, EncFlow m r) => [Id Domain.Person] -> m [Domain.Person]
castFindPersonInIds mdPersonIds = do
  drivers' <- findDriverByIdsIn (map cast mdPersonIds)
  drivers <- mapM decrypt drivers'
  pure $ map castPerson drivers
  where
    castPerson person = Domain.Person (cast person.id) person.firstName person.mobileNumber

castFindAllDriverIdExceptProvided :: Transactionable m => Id Domain.Merchant -> [Id Domain.Person] -> m [Id Domain.Person]
castFindAllDriverIdExceptProvided mdMerchantId lsMdDriverId = map cast <$> findAllDriverIdExceptProvided (cast mdMerchantId) (map cast lsMdDriverId)

addLinkAsMedia :: ShortId DM.Merchant -> Common.AddLinkAsMedia -> FlowHandler Common.UploadFileResponse
addLinkAsMedia merchantShortId req = withFlowHandlerAPI $ do
  _ <- findMerchantByShortId merchantShortId
  DMessage.addLinkAsMedia req

uploadFile :: ShortId DM.Merchant -> Common.UploadFileRequest -> FlowHandler Common.UploadFileResponse
uploadFile merchantShortId commonFileReq = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  transporterConfig <- QTC.findByMerchantId merchant.id
  DMessage.uploadFile commonFileReq (cast merchant.id) transporterConfig

addMessage :: ShortId DM.Merchant -> Common.AddMessageRequest -> FlowHandler Common.AddMessageResponse
addMessage merchantShortId req = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  DMessage.addMessage req (cast merchant.id)

sendMessage :: ShortId DM.Merchant -> Common.SendMessageRequest -> FlowHandler APISuccess
sendMessage merchantShortId req = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  DMessage.sendMessage req (cast merchant.id) sendMessageHandle

messageList :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> FlowHandler Common.MessageListResponse
messageList merchantShortId mbLimit mbOffset = withFlowHandlerAPI $ do
  merchant <- findMerchantByShortId merchantShortId
  DMessage.messageList mbLimit mbOffset (cast merchant.id)

messageInfo :: ShortId DM.Merchant -> Id Common.Message -> FlowHandler Common.MessageInfoResponse
messageInfo merchantShortId msgId = withFlowHandlerAPI $ do
  _ <- findMerchantByShortId merchantShortId
  DMessage.messageInfo (cast msgId)

messageDeliveryInfo :: ShortId DM.Merchant -> Id Common.Message -> FlowHandler Common.MessageDeliveryInfoResponse
messageDeliveryInfo merchantShortId msgId = withFlowHandlerAPI $ do
  _ <- findMerchantByShortId merchantShortId
  DMessage.messageDeliveryInfo (cast msgId)

messageReceiverList :: ShortId DM.Merchant -> Id Common.Message -> Maybe Text -> Maybe Common.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> FlowHandler Common.MessageReceiverListResponse
messageReceiverList _ messageId _ status mbOffset mbLimit = withFlowHandlerAPI $ do
  DMessage.messageReceiverList sendMessageHandle (cast messageId) status mbOffset mbLimit
