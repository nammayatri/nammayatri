{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Communication
  ( API.Types.ProviderPlatform.Management.Communication.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Communication
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Communication
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Communication.API)
handler merchantId city = postCommunicationCreate merchantId city :<|> getCommunicationList merchantId city :<|> getCommunicationInfo merchantId city :<|> postCommunicationSend merchantId city :<|> putCommunicationEdit merchantId city :<|> deleteCommunicationDelete merchantId city :<|> getCommunicationDeliveryStatus merchantId city :<|> getCommunicationRecipients merchantId city :<|> getCommunicationTemplate merchantId city

postCommunicationCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Person -> API.Types.ProviderPlatform.Management.Communication.CreateCommunicationRequest -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.CreateCommunicationResponse)
postCommunicationCreate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.postCommunicationCreate a4 a3 a2 a1

getCommunicationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Communication.CommunicationListType -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Communication.CommunicationChannelType -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Communication.CommunicationDomainType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.CommunicationListResponse)
getCommunicationList a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.getCommunicationList a9 a8 a7 a6 a5 a4 a3 a2 a1

getCommunicationInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Communication -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.CommunicationInfoResponse)
getCommunicationInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.getCommunicationInfo a3 a2 a1

postCommunicationSend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Communication -> API.Types.ProviderPlatform.Management.Communication.SendCommunicationRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCommunicationSend a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.postCommunicationSend a4 a3 a2 a1

putCommunicationEdit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Communication -> API.Types.ProviderPlatform.Management.Communication.EditCommunicationRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putCommunicationEdit a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.putCommunicationEdit a4 a3 a2 a1

deleteCommunicationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Communication -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteCommunicationDelete a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.deleteCommunicationDelete a3 a2 a1

getCommunicationDeliveryStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Communication -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Communication.CommunicationChannelType -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Communication.CommunicationDeliveryStatusType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.DeliveryStatusResponse)
getCommunicationDeliveryStatus a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.getCommunicationDeliveryStatus a7 a6 a5 a4 a3 a2 a1

getCommunicationRecipients :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Communication.CommunicationRoleType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.RecipientsResponse)
getCommunicationRecipients a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.getCommunicationRecipients a9 a8 a7 a6 a5 a4 a3 a2 a1

getCommunicationTemplate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Communication.CommunicationDomainType -> API.Types.ProviderPlatform.Management.Communication.CommunicationChannelType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.CommunicationTemplateResponse)
getCommunicationTemplate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.getCommunicationTemplate a4 a3 a2 a1
