{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Message
  ( API.Types.ProviderPlatform.Management.Message.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Message.API)
handler merchantId city = postMessageUploadFile merchantId city :<|> postMessageAddLink merchantId city :<|> postMessageAdd merchantId city :<|> postMessageSend merchantId city :<|> postMessageEdit merchantId city :<|> getMessageList merchantId city :<|> getMessageInfo merchantId city :<|> getMessageDeliveryInfo merchantId city :<|> getMessageReceiverList merchantId city

postMessageUploadFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Message.UploadFileRequest -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.UploadFileResponse)
postMessageUploadFile a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.postMessageUploadFile a3 a2 a1

postMessageAddLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Message.AddLinkAsMedia -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.UploadFileResponse)
postMessageAddLink a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.postMessageAddLink a3 a2 a1

postMessageAdd :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Message.AddMessageRequest -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.AddMessageResponse)
postMessageAdd a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.postMessageAdd a3 a2 a1

postMessageSend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Message.SendMessageRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMessageSend a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.postMessageSend a3 a2 a1

postMessageEdit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Message.EditMessageRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMessageEdit a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.postMessageEdit a3 a2 a1

getMessageList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.MessageListResponse)
getMessageList a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.getMessageList a4 a3 a2 a1

getMessageInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Message -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.MessageInfoResponse)
getMessageInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.getMessageInfo a3 a2 a1

getMessageDeliveryInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Message -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.MessageDeliveryInfoResponse)
getMessageDeliveryInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.getMessageDeliveryInfo a3 a2 a1

getMessageReceiverList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Message -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Message.MessageDeliveryStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Message.MessageReceiverListResponse)
getMessageReceiverList a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Message.getMessageReceiverList a7 a6 a5 a4 a3 a2 a1
