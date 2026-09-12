{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Communication
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Communication
import qualified Dashboard.Common
import qualified Data.Time.Calendar
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
import Tools.Auth.DashboardUserAuth

type API = ("communication" :> (PostCommunicationCreate :<|> GetCommunicationList :<|> GetCommunicationInfo :<|> PostCommunicationSend :<|> PutCommunicationEdit :<|> DeleteCommunicationDelete :<|> GetCommunicationDeliveryStatus :<|> GetCommunicationRecipients :<|> GetCommunicationTemplate))

type PostCommunicationCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/COMMUNICATION/POST_COMMUNICATION_CREATE"
      :> API.Types.ProviderPlatform.Management.Communication.PostCommunicationCreate
  )

type GetCommunicationList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_LIST"
      :> API.Types.ProviderPlatform.Management.Communication.GetCommunicationList
  )

type GetCommunicationInfo =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_INFO"
      :> API.Types.ProviderPlatform.Management.Communication.GetCommunicationInfo
  )

type PostCommunicationSend =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/COMMUNICATION/POST_COMMUNICATION_SEND"
      :> API.Types.ProviderPlatform.Management.Communication.PostCommunicationSend
  )

type PutCommunicationEdit =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/COMMUNICATION/PUT_COMMUNICATION_EDIT"
      :> API.Types.ProviderPlatform.Management.Communication.PutCommunicationEdit
  )

type DeleteCommunicationDelete =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/COMMUNICATION/DELETE_COMMUNICATION_DELETE"
      :> API.Types.ProviderPlatform.Management.Communication.DeleteCommunicationDelete
  )

type GetCommunicationDeliveryStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_DELIVERY_STATUS"
      :> API.Types.ProviderPlatform.Management.Communication.GetCommunicationDeliveryStatus
  )

type GetCommunicationRecipients =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_RECIPIENTS"
      :> API.Types.ProviderPlatform.Management.Communication.GetCommunicationRecipients
  )

type GetCommunicationTemplate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_TEMPLATE"
      :> API.Types.ProviderPlatform.Management.Communication.GetCommunicationTemplate
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postCommunicationCreate merchantId city :<|> getCommunicationList merchantId city :<|> getCommunicationInfo merchantId city :<|> postCommunicationSend merchantId city :<|> putCommunicationEdit merchantId city :<|> deleteCommunicationDelete merchantId city :<|> getCommunicationDeliveryStatus merchantId city :<|> getCommunicationRecipients merchantId city :<|> getCommunicationTemplate merchantId city

postCommunicationCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Person -> API.Types.ProviderPlatform.Management.Communication.CreateCommunicationRequest -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.CreateCommunicationResponse)
postCommunicationCreate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.postCommunicationCreate a5 a4 a2 a1

getCommunicationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Communication.CommunicationListType) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Communication.CommunicationChannelType) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Communication.CommunicationDomainType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Data.Time.Calendar.Day) -> Kernel.Prelude.Maybe (Data.Time.Calendar.Day) -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.CommunicationListResponse)
getCommunicationList a12 a11 _a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.getCommunicationList a12 a11 a9 a8 a7 a6 a5 a4 a3 a2 a1

getCommunicationInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Communication -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.CommunicationInfoResponse)
getCommunicationInfo a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.getCommunicationInfo a4 a3 a1

postCommunicationSend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Communication -> API.Types.ProviderPlatform.Management.Communication.SendCommunicationRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCommunicationSend a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.postCommunicationSend a5 a4 a2 a1

putCommunicationEdit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Communication -> API.Types.ProviderPlatform.Management.Communication.EditCommunicationRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putCommunicationEdit a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.putCommunicationEdit a5 a4 a2 a1

deleteCommunicationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Communication -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteCommunicationDelete a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.deleteCommunicationDelete a4 a3 a1

getCommunicationDeliveryStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Communication -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Communication.CommunicationChannelType) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Communication.CommunicationDeliveryStatusType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.DeliveryStatusResponse)
getCommunicationDeliveryStatus a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.getCommunicationDeliveryStatus a8 a7 a5 a4 a3 a2 a1

getCommunicationRecipients :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Communication.CommunicationRoleType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.RecipientsResponse)
getCommunicationRecipients a10 a9 _a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.getCommunicationRecipients a10 a9 a7 a6 a5 a4 a3 a2 a1

getCommunicationTemplate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Communication.CommunicationDomainType -> API.Types.ProviderPlatform.Management.Communication.CommunicationChannelType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.CommunicationTemplateResponse)
getCommunicationTemplate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Communication.getCommunicationTemplate a5 a4 a2 a1
