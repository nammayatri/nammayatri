{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Roles where

import Control.Applicative ((<|>))
import Dashboard.Common
import qualified Domain.Types.AccessMatrix as DMatrix
import Domain.Types.Role
import qualified Domain.Types.Role as DRole
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import qualified SharedLogic.Roles as SRoles
import Storage.Beam.BeamFlow (BeamFlow)
import qualified Storage.CachedQueries.Role as CQRole
import qualified Storage.Queries.AccessMatrix as QMatrix
import Tools.Auth
import Tools.Error (GenericError (..), RoleError (..))

data CreateRoleReq = CreateRoleReq
  { name :: Text,
    dashboardAccessType :: Maybe DashboardAccessType,
    parentRoleId :: Maybe (Id DRole.Role),
    description :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data UpdateRoleReq = UpdateRoleReq
  { roleId :: Id DRole.Role,
    name :: Maybe Text,
    dashboardAccessType :: Maybe DashboardAccessType,
    parentRoleId :: Maybe (Id DRole.Role),
    description :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AssignAccessLevelReq = AssignAccessLevelReq
  { apiEntity :: DMatrix.ApiEntity,
    userActionType :: DMatrix.UserActionTypeWrapper,
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
  runRequestValidation validateCreateRoleReq req
  mbExistingRole <- CQRole.findByName req.name
  whenJust mbExistingRole $ \_ -> throwError (RoleNameExists req.name)
  role <- buildRole req
  -- void $ CQRole.cacheParentRolesRecursively role

  case req.parentRoleId of
    Just parentRoleId -> do
      -- fetch roles, including updated role
      allRoles <- CQRole.findAll
      _parentRole <- find (\r -> r.id == parentRoleId) allRoles & fromMaybeM (RoleDoesNotExist parentRoleId.getId)
      let allUpdRoles = role : allRoles
      case SRoles.calculateRoleHierarchy allUpdRoles of
        Left (SRoles.CycleDetected cycleRoles) -> throwError (InvalidRequest $ "Cycle detected in roles hierarchy: " <> show cycleRoles)
        Right rolesHierarchy -> do
          CQRole.create role
          CQRole.cacheRoleHierarchy rolesHierarchy
    Nothing -> do
      -- no need to update whole cache as hierarchy did not changed
      let newRoleHierarchy =
            pure
              SRoles.RoleHierarchy
                { role,
                  roleAncestors = [],
                  roleDescendants = []
                }
      CQRole.create role
      CQRole.cacheRoleHierarchy newRoleHierarchy

  pure $ DRole.mkRoleAPIEntity role

-- Validate input fields
validateCreateRoleReq :: Validate CreateRoleReq
validateCreateRoleReq CreateRoleReq {..} =
  sequenceA_
    [ validateField "name" name $ MinLength 3 `And` MaxLength 50 `And` P.name,
      validateField "description" description $ MinLength 3 `And` P.inputName
    ]

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
        dashboardAccessType = fromMaybe DRole.DASHBOARD_USER req.dashboardAccessType,
        parentRoleId = req.parentRoleId,
        description = req.description,
        createdAt = now,
        updatedAt = now
      }

updateRole ::
  BeamFlow m r =>
  TokenInfo ->
  UpdateRoleReq ->
  m DRole.RoleAPIEntity
updateRole _ req = do
  -- runRequestValidation validateUpdateRoleReq req

  whenJust req.name $ \reqName -> do
    mbExistingRole <- CQRole.findByName reqName
    whenJust mbExistingRole $ \existingRole -> do
      unless (existingRole.id == req.roleId) $
        throwError (RoleNameExists reqName)

  role <- CQRole.findById req.roleId >>= fromMaybeM (RoleDoesNotExist req.roleId.getId)

  let updRole =
        role{name = fromMaybe role.name req.name,
             dashboardAccessType = fromMaybe role.dashboardAccessType req.dashboardAccessType,
             parentRoleId = req.parentRoleId <|> role.parentRoleId,
             description = fromMaybe role.description req.description
            }

  case req.parentRoleId of
    Just parentRoleId -> do
      -- fetch roles, including updated role
      allRoles <- CQRole.findAll
      _parentRole <- find (\r -> r.id == parentRoleId) allRoles & fromMaybeM (RoleDoesNotExist parentRoleId.getId)
      let allUpdRoles = map (\r -> if r.id == updRole.id then updRole else r) allRoles
      case SRoles.calculateRoleHierarchy allUpdRoles of
        Left (SRoles.CycleDetected cycleRoles) -> throwError (InvalidRequest $ "Cycle detected in roles hierarchy: " <> show cycleRoles)
        Right rolesHierarchy -> do
          CQRole.updateById updRole
          CQRole.cacheRoleHierarchy rolesHierarchy
    Nothing ->
      -- no need to update cache as hierarchy did not changed
      CQRole.updateById updRole
  pure $ DRole.mkRoleAPIEntity updRole

assignAccessLevel ::
  BeamFlow m r =>
  TokenInfo ->
  Id DRole.Role ->
  AssignAccessLevelReq ->
  m APISuccess
assignAccessLevel _ roleId req = do
  _role <- CQRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
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
        -- isDerived = False,
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
  personAndRoleList <- B.runInReplica $ CQRole.findAllWithLimitOffset mbLimit mbOffset mbSearchString
  res <- forM personAndRoleList $ \role -> do
    pure $ mkRoleAPIEntity role
  let count = length res
  let summary = Summary {totalCount = 10000, count}
  pure $ ListRoleRes {list = res, summary = summary}
