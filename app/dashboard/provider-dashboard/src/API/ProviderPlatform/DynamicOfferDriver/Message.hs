module API.ProviderPlatform.DynamicOfferDriver.Message
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI)
import qualified ProviderPlatformClient.DynamicOfferDriver as Client
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import "lib-dashboard" Tools.Auth hiding (BECKN_TRANSPORT)
import "lib-dashboard" Tools.Auth.Merchant

type API =
  "message"
    :> ( UploadFileAPI
           :<|> AddMessageAPI
           :<|> SendMessageAPI
           :<|> MessageListAPI
           :<|> MessageInfoAPI
           :<|> MessageDeliveryInfoAPI
           :<|> MessageReceiverListAPI
       )

type UploadFileAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MESSAGE
    :> Common.UploadFileAPI

type AddMessageAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MESSAGE
    :> Common.AddMessageAPI

type SendMessageAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'WRITE_ACCESS 'MESSAGE
    :> Common.SendMessageAPI

type MessageListAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'MESSAGE
    :> Common.MessageListAPI

type MessageInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'MESSAGE
    :> Common.MessageInfoAPI

type MessageDeliveryInfoAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'MESSAGE
    :> Common.MessageDeliveryInfoAPI

type MessageReceiverListAPI =
  ApiAuth 'DRIVER_OFFER_BPP 'READ_ACCESS 'MESSAGE
    :> Common.MessageReceiverListAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  uploadFile merchantId
    :<|> addMessage merchantId
    :<|> sendMessage merchantId
    :<|> messageList merchantId
    :<|> messageInfo merchantId
    :<|> messageDeliveryInfo merchantId
    :<|> messageReceiverList merchantId

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  Common.MessageEndpoint ->
  ApiTokenInfo ->
  Maybe request ->
  m DT.Transaction
buildTransaction endpoint apiTokenInfo =
  T.buildTransaction (DT.MessageAPI endpoint) apiTokenInfo Nothing Nothing

uploadFile :: ShortId DM.Merchant -> ApiTokenInfo -> Common.UploadFileRequest -> FlowHandler Common.UploadFileResponse
uploadFile merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.UploadFileEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (addMultipartBoundary . (.message.uploadFile)) req
  where
    addMultipartBoundary clientFn reqBody = clientFn ("XXX00XXX", reqBody)

addMessage :: ShortId DM.Merchant -> ApiTokenInfo -> Common.AddMessageRequest -> FlowHandler Common.AddMessageResponse
addMessage merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.AddMessageEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (.message.addMessage) req

sendMessage :: ShortId DM.Merchant -> ApiTokenInfo -> Common.SendMessageRequest -> FlowHandler APISuccess
sendMessage merchantShortId apiTokenInfo req = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  transaction <- buildTransaction Common.SendMessageEndpoint apiTokenInfo T.emptyRequest
  T.withTransactionStoring transaction $
    Client.callDriverOfferBPP checkedMerchantId (addMultipartBoundary . (.message.sendMessage)) req
  where
    addMultipartBoundary clientFn reqBody = clientFn ("XXX00XXX", reqBody)

messageList :: ShortId DM.Merchant -> ApiTokenInfo -> Maybe Int -> Maybe Int -> FlowHandler Common.MessageListResponse
messageList merchantShortId apiTokenInfo mbLimit mbOffset = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.message.messageList) mbLimit mbOffset

messageInfo :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Message -> FlowHandler Common.MessageInfoResponse
messageInfo merchantShortId apiTokenInfo messageId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.message.messageInfo) messageId

messageDeliveryInfo :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Message -> FlowHandler Common.MessageDeliveryInfoResponse
messageDeliveryInfo merchantShortId apiTokenInfo messageId = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.message.messageDeliveryInfo) messageId

messageReceiverList :: ShortId DM.Merchant -> ApiTokenInfo -> Id Common.Message -> Maybe Text -> Maybe Common.MessageDeliveryStatus -> Maybe Int -> Maybe Int -> FlowHandler Common.MessageReceiverListResponse
messageReceiverList merchantShortId apiTokenInfo messageId number status limit offset = withFlowHandlerAPI $ do
  checkedMerchantId <- merchantAccessCheck merchantShortId apiTokenInfo.merchant.shortId
  Client.callDriverOfferBPP checkedMerchantId (.message.messageReceiverList) messageId number status limit offset
