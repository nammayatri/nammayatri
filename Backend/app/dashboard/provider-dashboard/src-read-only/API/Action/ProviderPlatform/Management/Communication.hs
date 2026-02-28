{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.Communication
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Communication
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.Communication
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

type API = ("communication" :> (PostCommunicationCreate :<|> GetCommunicationList :<|> GetCommunicationInfo :<|> PostCommunicationSend :<|> PutCommunicationEdit :<|> DeleteCommunicationDelete :<|> GetCommunicationDeliveryStatus :<|> GetCommunicationRecipients :<|> GetCommunicationTemplate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postCommunicationCreate merchantId city :<|> getCommunicationList merchantId city :<|> getCommunicationInfo merchantId city :<|> postCommunicationSend merchantId city :<|> putCommunicationEdit merchantId city :<|> deleteCommunicationDelete merchantId city :<|> getCommunicationDeliveryStatus merchantId city :<|> getCommunicationRecipients merchantId city :<|> getCommunicationTemplate merchantId city

type PostCommunicationCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.COMMUNICATION / 'API.Types.ProviderPlatform.Management.Communication.POST_COMMUNICATION_CREATE)
      :> API.Types.ProviderPlatform.Management.Communication.PostCommunicationCreate
  )

type GetCommunicationList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.COMMUNICATION / 'API.Types.ProviderPlatform.Management.Communication.GET_COMMUNICATION_LIST)
      :> API.Types.ProviderPlatform.Management.Communication.GetCommunicationList
  )

type GetCommunicationInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.COMMUNICATION / 'API.Types.ProviderPlatform.Management.Communication.GET_COMMUNICATION_INFO)
      :> API.Types.ProviderPlatform.Management.Communication.GetCommunicationInfo
  )

type PostCommunicationSend =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.COMMUNICATION / 'API.Types.ProviderPlatform.Management.Communication.POST_COMMUNICATION_SEND)
      :> API.Types.ProviderPlatform.Management.Communication.PostCommunicationSend
  )

type PutCommunicationEdit =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.COMMUNICATION / 'API.Types.ProviderPlatform.Management.Communication.PUT_COMMUNICATION_EDIT)
      :> API.Types.ProviderPlatform.Management.Communication.PutCommunicationEdit
  )

type DeleteCommunicationDelete =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.COMMUNICATION / 'API.Types.ProviderPlatform.Management.Communication.DELETE_COMMUNICATION_DELETE)
      :> API.Types.ProviderPlatform.Management.Communication.DeleteCommunicationDelete
  )

type GetCommunicationDeliveryStatus =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.COMMUNICATION / 'API.Types.ProviderPlatform.Management.Communication.GET_COMMUNICATION_DELIVERY_STATUS)
      :> API.Types.ProviderPlatform.Management.Communication.GetCommunicationDeliveryStatus
  )

type GetCommunicationRecipients =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.COMMUNICATION / 'API.Types.ProviderPlatform.Management.Communication.GET_COMMUNICATION_RECIPIENTS)
      :> API.Types.ProviderPlatform.Management.Communication.GetCommunicationRecipients
  )

type GetCommunicationTemplate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.COMMUNICATION / 'API.Types.ProviderPlatform.Management.Communication.GET_COMMUNICATION_TEMPLATE)
      :> API.Types.ProviderPlatform.Management.Communication.GetCommunicationTemplate
  )

postCommunicationCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Person -> API.Types.ProviderPlatform.Management.Communication.CreateCommunicationRequest -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.CreateCommunicationResponse)
postCommunicationCreate merchantShortId opCity apiTokenInfo personId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Communication.postCommunicationCreate merchantShortId opCity apiTokenInfo personId req

getCommunicationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Communication.CommunicationListType -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Communication.CommunicationChannelType -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Communication.CommunicationDomainType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.CommunicationListResponse)
getCommunicationList merchantShortId opCity apiTokenInfo listType channel domain search limit offset personId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Communication.getCommunicationList merchantShortId opCity apiTokenInfo listType channel domain search limit offset personId

getCommunicationInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Communication -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.CommunicationInfoResponse)
getCommunicationInfo merchantShortId opCity apiTokenInfo communicationId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Communication.getCommunicationInfo merchantShortId opCity apiTokenInfo communicationId

postCommunicationSend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Communication -> API.Types.ProviderPlatform.Management.Communication.SendCommunicationRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCommunicationSend merchantShortId opCity apiTokenInfo communicationId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Communication.postCommunicationSend merchantShortId opCity apiTokenInfo communicationId req

putCommunicationEdit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Communication -> API.Types.ProviderPlatform.Management.Communication.EditCommunicationRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putCommunicationEdit merchantShortId opCity apiTokenInfo communicationId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Communication.putCommunicationEdit merchantShortId opCity apiTokenInfo communicationId req

deleteCommunicationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Communication -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteCommunicationDelete merchantShortId opCity apiTokenInfo communicationId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Communication.deleteCommunicationDelete merchantShortId opCity apiTokenInfo communicationId

getCommunicationDeliveryStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Communication -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Communication.CommunicationChannelType -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Communication.CommunicationDeliveryStatusType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.DeliveryStatusResponse)
getCommunicationDeliveryStatus merchantShortId opCity apiTokenInfo communicationId channel status limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Communication.getCommunicationDeliveryStatus merchantShortId opCity apiTokenInfo communicationId channel status limit offset

getCommunicationRecipients :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Communication.CommunicationRoleType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.RecipientsResponse)
getCommunicationRecipients merchantShortId opCity apiTokenInfo role fleetOwnerId operatorId search selectAll limit offset = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Communication.getCommunicationRecipients merchantShortId opCity apiTokenInfo role fleetOwnerId operatorId search selectAll limit offset

getCommunicationTemplate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Communication.CommunicationDomainType -> API.Types.ProviderPlatform.Management.Communication.CommunicationChannelType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Communication.CommunicationTemplateResponse)
getCommunicationTemplate merchantShortId opCity apiTokenInfo domain channel = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.Communication.getCommunicationTemplate merchantShortId opCity apiTokenInfo domain channel
