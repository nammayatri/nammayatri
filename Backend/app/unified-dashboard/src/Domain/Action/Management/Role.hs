{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Management.Role
  ( postRoleCreate,
    postRoleAssignAccessLevel,
    getRoleList,
  )
where

import qualified API.Types.Management.Role
import qualified Data.Text
import qualified Data.Text as T
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Role as DRole
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.External.Verification.SafetyPortal.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as City
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.AccessMatrix as QMatrix
import qualified Storage.Queries.Role as QRole
import qualified Storage.Queries.RoleExtra as QRoleExtra
import Tools.Auth.Api
import Tools.Error

postRoleCreate ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  API.Types.Management.Role.CreateRoleReq ->
  Environment.Flow DRole.RoleAPIEntity
postRoleCreate _ _ _ req = do
  mbExistingRole <- B.runInReplica $ QRole.findByName req.name
  whenJust mbExistingRole $ \_ -> throwError (InvalidRequest $ "Role with name " <> req.name <> " already exists")

  role <- buildRole req
  QRole.create role
  pure $ mkRoleAPIEntity role

buildRole ::
  API.Types.Management.Role.CreateRoleReq ->
  Environment.Flow DRole.Role
buildRole req = do
  uid <- generateGUID
  now <- getCurrentTime
  pure
    DRole.Role
      { id = uid,
        name = req.name,
        description = req.description,
        needsBppAccountCreation = req.needsBppAccountCreation,
        createdAt = now,
        updatedAt = now
      }

postRoleAssignAccessLevel ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id DRole.Role ->
  API.Types.Management.Role.AssignAccessLevelReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postRoleAssignAccessLevel _ _ _ roleId req = do
  _role <- B.runInReplica $ QRole.findById roleId >>= fromMaybeM (InvalidRequest $ "Role with id " <> show roleId.getId <> " does not exist")

  mbAccessMatrixItem <- B.runInReplica $ QMatrix.findByRoleIdAndServerAndActionType roleId (Just req.serverName) req.userActionType
  case mbAccessMatrixItem of
    Just _accessMatrixItem -> pure Kernel.Types.APISuccess.Success -- Already exists, no update needed in new structure
    Nothing -> do
      accessMatrixItem <- buildAccessMatrixItem roleId req
      _ <- QMatrix.create accessMatrixItem
      pure Kernel.Types.APISuccess.Success

buildAccessMatrixItem ::
  Kernel.Types.Id.Id DRole.Role ->
  API.Types.Management.Role.AssignAccessLevelReq ->
  Environment.Flow DMatrix.AccessMatrix
buildAccessMatrixItem roleId req = do
  uid <- generateGUID
  now <- getCurrentTime
  pure
    DMatrix.AccessMatrix
      { id = uid,
        roleId = roleId,
        serverName = Just req.serverName,
        userActionType = req.userActionType,
        createdAt = now,
        updatedAt = now
      }

getRoleList ::
  Kernel.Types.Id.ShortId DMerchant.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Kernel.Prelude.Maybe Data.Text.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Integer ->
  Kernel.Prelude.Maybe Kernel.Prelude.Integer ->
  Environment.Flow API.Types.Management.Role.ListRoleResp
getRoleList _ _ _ mbSearchString mbLimit mbOffset = do
  roles <- B.runInReplica $ QRoleExtra.findAllRolesWithLimitOffset mbSearchString mbLimit mbOffset
  let res = map mkRoleAPIEntity roles
  let count = length res
  let summary = Kernel.External.Verification.SafetyPortal.Types.Summary {totalCount = 10000, count}
  pure $ API.Types.Management.Role.ListRoleResp {list = res, summary = summary}

mkRoleAPIEntity :: DRole.Role -> DRole.RoleAPIEntity
mkRoleAPIEntity role =
  DRole.RoleAPIEntity
    { id = role.id,
      name = role.name,
      description = role.description,
      needsBppAccountCreation = role.needsBppAccountCreation
    }
