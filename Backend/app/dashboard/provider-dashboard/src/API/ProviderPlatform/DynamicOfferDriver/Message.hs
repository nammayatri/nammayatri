{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver.Message
  ( API,
    handler,
  )
where

import AWS.S3 (FileType (..))
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified Data.Text as DT
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, throwError, withFlowHandlerAPI')
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Tools.Auth
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "message"
    :> ( UploadFileAPI
           :<|> AddLinkAPI
           :<|> AddMessageAPI
           :<|> SendMessageAPI
           :<|> MessageListAPI
           :<|> MessageInfoAPI
           :<|> MessageDeliveryInfoAPI
           :<|> MessageReceiverListAPI
       )

type UploadFileAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MESSAGE 'UPLOAD_FILE
    :> Common.UploadFileAPI

type AddLinkAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MESSAGE 'ADD_LINK
    :> Common.AddLinkAPI

type AddMessageAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MESSAGE 'ADD_MESSAGE
    :> Common.AddMessageAPI

type SendMessageAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MESSAGE 'SEND_MESSAGE
    :> Common.SendMessageAPI

type MessageListAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MESSAGE 'MESSAGE_LIST
    :> Common.MessageListAPI

type MessageInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MESSAGE 'MESSAGE_INFO
    :> Common.MessageInfoAPI

type MessageDeliveryInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MESSAGE 'MESSAGE_DELIVERY_INFO
    :> Common.MessageDeliveryInfoAPI

type MessageReceiverListAPI =
  ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'MESSAGE 'MESSAGE_RECEIVER_LIST
    :> Common.MessageReceiverListAPI

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

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.MessageEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.MessageAPI endpoint) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) Nothing Nothing

addLinkAsMedia :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.AddLinkAsMedia -> FlowHandler Common.UploadFileResponse
addLinkAsMedia merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (((req.fileType == VideoLink || req.fileType == PortraitVideoLink) && checkIfYoutubeLink req.url) || req.fileType == ImageLink) $
    throwError $ InvalidRequest "Only support youtube video links and image links. For Audio use uploadFile API."
  transaction <- buildTransaction Common.AddLinkEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.message.addLinkAsMedia) req
  where
    -- youtube link can be https://youtu.be/shorts/nWbI-DfwRpw or https://www.youtube.com/shorts/nWbI-DfwRpw
    checkIfYoutubeLink link = DT.isPrefixOf "https://" link && DT.isInfixOf "youtu" link

uploadFile :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.UploadFileRequest -> FlowHandler Common.UploadFileResponse
uploadFile merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (req.fileType `elem` [Audio, Image]) $
    throwError $ InvalidRequest "Only support Audio/Image media type. For Video/MediaLinks use AddLink API."
  transaction <- buildTransaction Common.UploadFileEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (addMultipartBoundary . (.message.uploadFile)) req
  where
    addMultipartBoundary clientFn reqBody = clientFn ("XXX00XXX", reqBody)

addMessage :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.AddMessageRequest -> FlowHandler Common.AddMessageResponse
addMessage merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  unless (length req.mediaFiles <= 1) $
    throwError $ InvalidRequest "Only support one media file per message. More than one media support will be added soon."
  transaction <- buildTransaction Common.AddMessageEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (.message.addMessage) req

sendMessage :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.SendMessageRequest -> FlowHandler APISuccess
sendMessage merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction Common.SendMessageEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPPOperations checkedMerchantId opCity (addMultipartBoundary . (.message.sendMessage)) req
  where
    addMultipartBoundary clientFn reqBody = clientFn ("XXX00XXX", reqBody)

messageList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> FlowHandler Common.MessageListResponse
messageList merchantShortId opCity apiTokenInfo mbLimit mbOffset = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.message.messageList) mbLimit mbOffset

messageInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Message -> FlowHandler Common.MessageInfoResponse
messageInfo merchantShortId opCity apiTokenInfo messageId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.message.messageInfo) messageId

messageDeliveryInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Message -> FlowHandler Common.MessageDeliveryInfoResponse
messageDeliveryInfo merchantShortId opCity apiTokenInfo messageId = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.message.messageDeliveryInfo) messageId

messageReceiverList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Message -> Maybe Text -> Maybe Common.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> FlowHandler Common.MessageReceiverListResponse
messageReceiverList merchantShortId opCity apiTokenInfo messageId number status limit offset = withFlowHandlerAPI' $ do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callDriverOfferBPPOperations checkedMerchantId opCity (.message.messageReceiverList) messageId number status limit offset
