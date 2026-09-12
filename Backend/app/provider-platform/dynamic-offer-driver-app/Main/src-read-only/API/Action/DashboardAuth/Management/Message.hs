{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Message
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Message
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Message
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("message" :> (PostMessageUploadFile :<|> PostMessageAddLink :<|> PostMessageAdd :<|> PostMessageSend :<|> PostMessageEdit :<|> GetMessageList :<|> GetMessageInfo :<|> GetMessageDeliveryInfo :<|> GetMessageReceiverList))

type PostMessageUploadFile =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_UPLOAD_FILE"
      :> API.Types.ProviderPlatform.Management.Message.PostMessageUploadFile
  )

type PostMessageAddLink = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_ADD_LINK" :> API.Types.ProviderPlatform.Management.Message.PostMessageAddLink)

type PostMessageAdd = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_ADD" :> API.Types.ProviderPlatform.Management.Message.PostMessageAdd)

type PostMessageSend = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_SEND" :> API.Types.ProviderPlatform.Management.Message.PostMessageSend)

type PostMessageEdit = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_EDIT" :> API.Types.ProviderPlatform.Management.Message.PostMessageEdit)

type GetMessageList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/MESSAGE/GET_MESSAGE_LIST" :> API.Types.ProviderPlatform.Management.Message.GetMessageList)

type GetMessageInfo = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/MESSAGE/GET_MESSAGE_INFO" :> API.Types.ProviderPlatform.Management.Message.GetMessageInfo)

type GetMessageDeliveryInfo =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MESSAGE/GET_MESSAGE_DELIVERY_INFO"
      :> API.Types.ProviderPlatform.Management.Message.GetMessageDeliveryInfo
  )

type GetMessageReceiverList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/MESSAGE/GET_MESSAGE_RECEIVER_LIST"
      :> API.Types.ProviderPlatform.Management.Message.GetMessageReceiverList
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postMessageUploadFile merchantId city :<|> postMessageAddLink merchantId city :<|> postMessageAdd merchantId city :<|> postMessageSend merchantId city :<|> postMessageEdit merchantId city :<|> getMessageList merchantId city :<|> getMessageInfo merchantId city :<|> getMessageDeliveryInfo merchantId city :<|> getMessageReceiverList merchantId city

postMessageUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Message.UploadFileRequest -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.UploadFileResponse)
postMessageUploadFile a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.postMessageUploadFile a4 a3 a1

postMessageAddLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Message.AddLinkAsMedia -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.UploadFileResponse)
postMessageAddLink a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.postMessageAddLink a4 a3 a1

postMessageAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Message.AddMessageRequest -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.AddMessageResponse)
postMessageAdd a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.postMessageAdd a4 a3 a1

postMessageSend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Message.SendMessageRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMessageSend a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.postMessageSend a4 a3 a1

postMessageEdit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Message.EditMessageRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMessageEdit a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.postMessageEdit a4 a3 a1

getMessageList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.MessageListResponse)
getMessageList a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.getMessageList a5 a4 a2 a1

getMessageInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Message -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.MessageInfoResponse)
getMessageInfo a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.getMessageInfo a4 a3 a1

getMessageDeliveryInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Message -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.MessageDeliveryInfoResponse)
getMessageDeliveryInfo a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.getMessageDeliveryInfo a4 a3 a1

getMessageReceiverList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Message -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Message.MessageDeliveryStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.MessageReceiverListResponse)
getMessageReceiverList a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.getMessageReceiverList a8 a7 a5 a4 a3 a2 a1
