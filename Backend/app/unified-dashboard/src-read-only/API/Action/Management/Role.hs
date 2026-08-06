{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Management.Role
  ( API,
    handler,
  )
where

import qualified "this" API.Types.Management
import qualified "this" API.Types.Management.Role
import qualified Data.Text
import qualified Domain.Action.Management.Role
import qualified Domain.Types.Merchant
import qualified Domain.Types.Role
import qualified Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("/role" :> (PostRoleCreate :<|> PostRoleAssignAccessLevel :<|> GetRoleList))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postRoleCreate merchantId city :<|> postRoleAssignAccessLevel merchantId city :<|> getRoleList merchantId city

type PostRoleCreate = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('POST_MANAGEMENT_ROLE_CREATE) :> API.Types.Management.Role.PostRoleCreate)

type PostRoleAssignAccessLevel = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('POST_MANAGEMENT_ROLE_ASSIGN_ACCESS_LEVEL) :> API.Types.Management.Role.PostRoleAssignAccessLevel)

type GetRoleList = (ApiAuth ('DRIVER_OFFER_BPP_MANAGEMENT) ('GET_MANAGEMENT_ROLE_LIST) :> API.Types.Management.Role.GetRoleList)

postRoleCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Management.Role.CreateRoleReq -> Environment.FlowHandler Domain.Types.Role.RoleAPIEntity)
postRoleCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.Management.Role.postRoleCreate merchantShortId opCity apiTokenInfo req

postRoleAssignAccessLevel :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Role.Role -> API.Types.Management.Role.AssignAccessLevelReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRoleAssignAccessLevel merchantShortId opCity apiTokenInfo roleId req = withFlowHandlerAPI' $ Domain.Action.Management.Role.postRoleAssignAccessLevel merchantShortId opCity apiTokenInfo roleId req

getRoleList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Environment.FlowHandler API.Types.Management.Role.ListRoleResp)
getRoleList merchantShortId opCity apiTokenInfo searchString limit offset = withFlowHandlerAPI' $ Domain.Action.Management.Role.getRoleList merchantShortId opCity apiTokenInfo searchString limit offset
