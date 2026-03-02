{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.Communication
  ( postCommunicationCreate,
    getCommunicationList,
    getCommunicationInfo,
    postCommunicationSend,
    putCommunicationEdit,
    deleteCommunicationDelete,
    getCommunicationDeliveryStatus,
    getCommunicationRecipients,
    getCommunicationTemplate,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.Communication
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postCommunicationCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Person -> API.Types.ProviderPlatform.Management.Communication.CreateCommunicationRequest -> Environment.Flow API.Types.ProviderPlatform.Management.Communication.CreateCommunicationResponse)
postCommunicationCreate merchantShortId opCity apiTokenInfo personId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.communicationDSL.postCommunicationCreate) personId req)

getCommunicationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Communication.CommunicationListType) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Communication.CommunicationChannelType) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Communication.CommunicationDomainType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Types.Id.Id Dashboard.Common.Person -> Environment.Flow API.Types.ProviderPlatform.Management.Communication.CommunicationListResponse)
getCommunicationList merchantShortId opCity apiTokenInfo listType channel domain search limit offset personId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.communicationDSL.getCommunicationList) listType channel domain search limit offset personId

getCommunicationInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Communication -> Environment.Flow API.Types.ProviderPlatform.Management.Communication.CommunicationInfoResponse)
getCommunicationInfo merchantShortId opCity apiTokenInfo communicationId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.communicationDSL.getCommunicationInfo) communicationId

postCommunicationSend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Communication -> API.Types.ProviderPlatform.Management.Communication.SendCommunicationRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postCommunicationSend merchantShortId opCity apiTokenInfo communicationId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.communicationDSL.postCommunicationSend) communicationId req)

putCommunicationEdit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Communication -> API.Types.ProviderPlatform.Management.Communication.EditCommunicationRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putCommunicationEdit merchantShortId opCity apiTokenInfo communicationId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.communicationDSL.putCommunicationEdit) communicationId req)

deleteCommunicationDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Communication -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
deleteCommunicationDelete merchantShortId opCity apiTokenInfo communicationId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.communicationDSL.deleteCommunicationDelete) communicationId)

getCommunicationDeliveryStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Communication -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Communication.CommunicationChannelType) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Communication.CommunicationDeliveryStatusType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.ProviderPlatform.Management.Communication.DeliveryStatusResponse)
getCommunicationDeliveryStatus merchantShortId opCity apiTokenInfo communicationId channel status limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.communicationDSL.getCommunicationDeliveryStatus) communicationId channel status limit offset

getCommunicationRecipients :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.Communication.CommunicationRoleType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.ProviderPlatform.Management.Communication.RecipientsResponse)
getCommunicationRecipients merchantShortId opCity apiTokenInfo role fleetOwnerId operatorId search selectAll limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.communicationDSL.getCommunicationRecipients) role fleetOwnerId operatorId search selectAll limit offset

getCommunicationTemplate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.Communication.CommunicationDomainType -> API.Types.ProviderPlatform.Management.Communication.CommunicationChannelType -> Environment.Flow API.Types.ProviderPlatform.Management.Communication.CommunicationTemplateResponse)
getCommunicationTemplate merchantShortId opCity apiTokenInfo domain channel = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.communicationDSL.getCommunicationTemplate) domain channel
