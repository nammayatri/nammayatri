{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Management.Endpoints.Role where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import qualified Domain.Types.AccessMatrix
import qualified Domain.Types.Role
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Verification.SafetyPortal.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data AssignAccessLevelReq = AssignAccessLevelReq {additionalUserActions :: Kernel.Prelude.Maybe Data.Text.Text, serverName :: Domain.Types.AccessMatrix.ServerName, userActionType :: Domain.Types.AccessMatrix.UserActionType}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateRoleReq = CreateRoleReq {description :: Data.Text.Text, name :: Data.Text.Text, needsBppAccountCreation :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ListRoleResp = ListRoleResp {list :: [Domain.Types.Role.RoleAPIEntity], summary :: Kernel.External.Verification.SafetyPortal.Types.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("/role" :> (PostRoleCreate :<|> PostRoleAssignAccessLevel :<|> GetRoleList))

type PostRoleCreate = ("create" :> ReqBody '[JSON] CreateRoleReq :> Post '[JSON] Domain.Types.Role.RoleAPIEntity)

type PostRoleAssignAccessLevel =
  ( Capture "roleId" (Kernel.Types.Id.Id Domain.Types.Role.Role) :> "assignAccessLevel" :> ReqBody '[JSON] AssignAccessLevelReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type GetRoleList = ("list" :> QueryParam "searchString" Data.Text.Text :> QueryParam "limit" Kernel.Prelude.Integer :> QueryParam "offset" Kernel.Prelude.Integer :> Get '[JSON] ListRoleResp)

data RoleAPIs = RoleAPIs
  { postRoleCreate :: CreateRoleReq -> EulerHS.Types.EulerClient Domain.Types.Role.RoleAPIEntity,
    postRoleAssignAccessLevel :: Kernel.Types.Id.Id Domain.Types.Role.Role -> AssignAccessLevelReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getRoleList :: Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> EulerHS.Types.EulerClient ListRoleResp
  }

mkRoleAPIs :: (Client EulerHS.Types.EulerClient API -> RoleAPIs)
mkRoleAPIs roleClient = (RoleAPIs {..})
  where
    postRoleCreate :<|> postRoleAssignAccessLevel :<|> getRoleList = roleClient

data RoleUserActionType
  = POST_ROLE_CREATE
  | POST_ROLE_ASSIGN_ACCESS_LEVEL
  | GET_ROLE_LIST
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''RoleUserActionType])
