{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Message
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Message
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.Message
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("message" :> (PostMessageUploadFile :<|> PostMessageAddLink :<|> PostMessageAdd :<|> PostMessageSend :<|> PostMessageEdit :<|> GetMessageList :<|> GetMessageInfo :<|> GetMessageDeliveryInfo :<|> GetMessageReceiverList))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postMessageUploadFile merchantId city :<|> postMessageAddLink merchantId city :<|> postMessageAdd merchantId city :<|> postMessageSend merchantId city :<|> postMessageEdit merchantId city :<|> getMessageList merchantId city :<|> getMessageInfo merchantId city :<|> getMessageDeliveryInfo merchantId city :<|> getMessageReceiverList merchantId city

type PostMessageUploadFile =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MESSAGE / 'API.Types.ProviderPlatform.Management.Message.POST_MESSAGE_UPLOAD_FILE)
      :> API.Types.ProviderPlatform.Management.Message.PostMessageUploadFile
  )

type PostMessageAddLink =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MESSAGE / 'API.Types.ProviderPlatform.Management.Message.POST_MESSAGE_ADD_LINK)
      :> API.Types.ProviderPlatform.Management.Message.PostMessageAddLink
  )

type PostMessageAdd =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MESSAGE / 'API.Types.ProviderPlatform.Management.Message.POST_MESSAGE_ADD)
      :> API.Types.ProviderPlatform.Management.Message.PostMessageAdd
  )

type PostMessageSend =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MESSAGE / 'API.Types.ProviderPlatform.Management.Message.POST_MESSAGE_SEND)
      :> API.Types.ProviderPlatform.Management.Message.PostMessageSend
  )

type PostMessageEdit =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MESSAGE / 'API.Types.ProviderPlatform.Management.Message.POST_MESSAGE_EDIT)
      :> API.Types.ProviderPlatform.Management.Message.PostMessageEdit
  )

type GetMessageList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MESSAGE / 'API.Types.ProviderPlatform.Management.Message.GET_MESSAGE_LIST)
      :> API.Types.ProviderPlatform.Management.Message.GetMessageList
  )

type GetMessageInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MESSAGE / 'API.Types.ProviderPlatform.Management.Message.GET_MESSAGE_INFO)
      :> API.Types.ProviderPlatform.Management.Message.GetMessageInfo
  )

type GetMessageDeliveryInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MESSAGE / 'API.Types.ProviderPlatform.Management.Message.GET_MESSAGE_DELIVERY_INFO)
      :> API.Types.ProviderPlatform.Management.Message.GetMessageDeliveryInfo
  )

type GetMessageReceiverList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.MESSAGE / 'API.Types.ProviderPlatform.Management.Message.GET_MESSAGE_RECEIVER_LIST)
      :> API.Types.ProviderPlatform.Management.Message.GetMessageReceiverList
  )

postMessageUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Message.UploadFileRequest -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.UploadFileResponse)
postMessageUploadFile merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Message.postMessageUploadFile merchantShortId opCity apiTokenInfo req

postMessageAddLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Message.AddLinkAsMedia -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.UploadFileResponse)
postMessageAddLink merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Message.postMessageAddLink merchantShortId opCity apiTokenInfo req

postMessageAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Message.AddMessageRequest -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.AddMessageResponse)
postMessageAdd merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Message.postMessageAdd merchantShortId opCity apiTokenInfo req

postMessageSend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Message.SendMessageRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMessageSend merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Message.postMessageSend merchantShortId opCity apiTokenInfo req

postMessageEdit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Message.EditMessageRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMessageEdit merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Message.postMessageEdit merchantShortId opCity apiTokenInfo req

getMessageList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.MessageListResponse)
getMessageList merchantShortId opCity apiTokenInfo limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Message.getMessageList merchantShortId opCity apiTokenInfo limit offset

getMessageInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Message -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.MessageInfoResponse)
getMessageInfo merchantShortId opCity apiTokenInfo messageId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Message.getMessageInfo merchantShortId opCity apiTokenInfo messageId

getMessageDeliveryInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Message -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.MessageDeliveryInfoResponse)
getMessageDeliveryInfo merchantShortId opCity apiTokenInfo messageId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Message.getMessageDeliveryInfo merchantShortId opCity apiTokenInfo messageId

getMessageReceiverList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Message -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Message.MessageDeliveryStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.MessageReceiverListResponse)
getMessageReceiverList merchantShortId opCity apiTokenInfo messageId number status limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Message.getMessageReceiverList merchantShortId opCity apiTokenInfo messageId number status limit offset
