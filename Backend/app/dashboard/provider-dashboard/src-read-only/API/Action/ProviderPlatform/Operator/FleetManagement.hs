{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Operator.FleetManagement
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2
import qualified API.Types.ProviderPlatform.Operator
import qualified API.Types.ProviderPlatform.Operator.FleetManagement
import qualified Domain.Action.ProviderPlatform.Operator.FleetManagement
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

type API = ("operator" :> (GetFleetManagementFleets :<|> PostFleetManagementFleetCreate :<|> PostFleetManagementFleetRegister :<|> PostFleetManagementFleetLinkSendOtp :<|> PostFleetManagementFleetLinkVerifyOtp :<|> PostFleetManagementFleetUnlink :<|> PostFleetManagementFleetMemberAssociationCreate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFleetManagementFleets merchantId city :<|> postFleetManagementFleetCreate merchantId city :<|> postFleetManagementFleetRegister merchantId city :<|> postFleetManagementFleetLinkSendOtp merchantId city :<|> postFleetManagementFleetLinkVerifyOtp merchantId city :<|> postFleetManagementFleetUnlink merchantId city :<|> postFleetManagementFleetMemberAssociationCreate merchantId city

type GetFleetManagementFleets =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.FLEET_MANAGEMENT / 'API.Types.ProviderPlatform.Operator.FleetManagement.GET_FLEET_MANAGEMENT_FLEETS)
      :> API.Types.ProviderPlatform.Operator.FleetManagement.GetFleetManagementFleets
  )

type PostFleetManagementFleetCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.FLEET_MANAGEMENT / 'API.Types.ProviderPlatform.Operator.FleetManagement.POST_FLEET_MANAGEMENT_FLEET_CREATE)
      :> API.Types.ProviderPlatform.Operator.FleetManagement.PostFleetManagementFleetCreate
  )

type PostFleetManagementFleetRegister =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.FLEET_MANAGEMENT / 'API.Types.ProviderPlatform.Operator.FleetManagement.POST_FLEET_MANAGEMENT_FLEET_REGISTER)
      :> API.Types.ProviderPlatform.Operator.FleetManagement.PostFleetManagementFleetRegister
  )

type PostFleetManagementFleetLinkSendOtp =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.FLEET_MANAGEMENT / 'API.Types.ProviderPlatform.Operator.FleetManagement.POST_FLEET_MANAGEMENT_FLEET_LINK_SEND_OTP)
      :> API.Types.ProviderPlatform.Operator.FleetManagement.PostFleetManagementFleetLinkSendOtp
  )

type PostFleetManagementFleetLinkVerifyOtp =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.FLEET_MANAGEMENT / 'API.Types.ProviderPlatform.Operator.FleetManagement.POST_FLEET_MANAGEMENT_FLEET_LINK_VERIFY_OTP)
      :> API.Types.ProviderPlatform.Operator.FleetManagement.PostFleetManagementFleetLinkVerifyOtp
  )

type PostFleetManagementFleetUnlink =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.FLEET_MANAGEMENT / 'API.Types.ProviderPlatform.Operator.FleetManagement.POST_FLEET_MANAGEMENT_FLEET_UNLINK)
      :> API.Types.ProviderPlatform.Operator.FleetManagement.PostFleetManagementFleetUnlink
  )

type PostFleetManagementFleetMemberAssociationCreate =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.FLEET_MANAGEMENT / 'API.Types.ProviderPlatform.Operator.FleetManagement.POST_FLEET_MANAGEMENT_FLEET_MEMBER_ASSOCIATION_CREATE)
      :> API.Types.ProviderPlatform.Operator.FleetManagement.PostFleetManagementFleetMemberAssociationCreate
  )

getFleetManagementFleets :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.FleetManagement.FleetInfoRes)
getFleetManagementFleets merchantShortId opCity apiTokenInfo isActive verified enabled limit offset mbSearchString = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.FleetManagement.getFleetManagementFleets merchantShortId opCity apiTokenInfo isActive verified enabled limit offset mbSearchString

postFleetManagementFleetCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerLoginReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.FleetManagement.postFleetManagementFleetCreate merchantShortId opCity apiTokenInfo req

postFleetManagementFleetRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerRegisterReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetRegister merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.FleetManagement.postFleetManagementFleetRegister merchantShortId opCity apiTokenInfo req

postFleetManagementFleetLinkSendOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerSendOtpReq -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerSendOtpRes)
postFleetManagementFleetLinkSendOtp merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.FleetManagement.postFleetManagementFleetLinkSendOtp merchantShortId opCity apiTokenInfo req

postFleetManagementFleetLinkVerifyOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerVerifyOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetLinkVerifyOtp merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.FleetManagement.postFleetManagementFleetLinkVerifyOtp merchantShortId opCity apiTokenInfo req

postFleetManagementFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetUnlink merchantShortId opCity apiTokenInfo fleetOwnerId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.FleetManagement.postFleetManagementFleetUnlink merchantShortId opCity apiTokenInfo fleetOwnerId

postFleetManagementFleetMemberAssociationCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetMemberAssociationCreateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetMemberAssociationCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.FleetManagement.postFleetManagementFleetMemberAssociationCreate merchantShortId opCity apiTokenInfo req
