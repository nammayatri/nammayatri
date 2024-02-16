{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Roles where

import Dashboard.Common
import qualified Domain.Types.AccessMatrix as DMatrix
import Domain.Types.Role
import qualified Domain.Types.Role as DRole
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow (BeamFlow)
import qualified Storage.Queries.AccessMatrix as QMatrix
import qualified Storage.Queries.Role as QRole
import Tools.Auth
import Tools.Error (RoleError (..))

data CreateRoleReq = CreateRoleReq
  { name :: Text,
    description :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AssignAccessLevelReq = AssignAccessLevelReq
  { apiEntity :: DMatrix.ApiEntity,
    userActionType :: DMatrix.UserActionType,
    userAccessType :: DMatrix.UserAccessType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ListRoleRes = ListRoleRes
  { list :: [RoleAPIEntity],
    summary :: Summary
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createRole ::
  BeamFlow m r =>
  TokenInfo ->
  CreateRoleReq ->
  m DRole.RoleAPIEntity
createRole _ req = do
  mbExistingRole <- QRole.findByName req.name
  whenJust mbExistingRole $ \_ -> throwError (RoleNameExists req.name)
  role <- buildRole req
  QRole.create role
  pure $ DRole.mkRoleAPIEntity role

buildRole ::
  MonadFlow m =>
  CreateRoleReq ->
  m DRole.Role
buildRole req = do
  uid <- generateGUID
  now <- getCurrentTime
  pure
    DRole.Role
      { id = uid,
        name = req.name,
        dashboardAccessType = DRole.DASHBOARD_USER,
        description = req.description,
        createdAt = now,
        updatedAt = now
      }

assignAccessLevel ::
  BeamFlow m r =>
  TokenInfo ->
  Id DRole.Role ->
  AssignAccessLevelReq ->
  m APISuccess
assignAccessLevel _ roleId req = do
  _role <- QRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  mbAccessMatrixItem <- QMatrix.findByRoleIdAndEntityAndActionType roleId req.apiEntity req.userActionType
  case mbAccessMatrixItem of
    Just accessMatrixItem -> QMatrix.updateUserAccessType accessMatrixItem.id req.userActionType req.userAccessType
    Nothing -> do
      accessMatrixItem <- buildAccessMatrixItem roleId req
      void $ QMatrix.create accessMatrixItem
  pure Success

buildAccessMatrixItem ::
  MonadFlow m =>
  Id DRole.Role ->
  AssignAccessLevelReq ->
  m DMatrix.AccessMatrixItem
buildAccessMatrixItem roleId req = do
  uid <- generateGUID
  now <- getCurrentTime
  pure
    DMatrix.AccessMatrixItem
      { id = uid,
        roleId = roleId,
        apiEntity = req.apiEntity,
        userActionType = req.userActionType,
        userAccessType = req.userAccessType,
        createdAt = now,
        updatedAt = now
      }

listRoles ::
  ( BeamFlow m r,
    EncFlow m r
  ) =>
  TokenInfo ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  m ListRoleRes
listRoles _ mbSearchString mbLimit mbOffset = do
  personAndRoleList <- B.runInReplica $ QRole.findAllWithLimitOffset mbLimit mbOffset mbSearchString
  res <- forM personAndRoleList $ \role -> do
    pure $ mkRoleAPIEntity role
  let count = length res
  let summary = Summary {totalCount = 10000, count}
  pure $ ListRoleRes {list = res, summary = summary}
