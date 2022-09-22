module Domain.Action.Dashboard.Roles where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Role as DRole
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
    userAccessType :: DMatrix.UserAccessType
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createRole ::
  EsqDBFlow m r =>
  TokenInfo ->
  CreateRoleReq ->
  m DRole.RoleAPIEntity
createRole _ req = do
  mbExistingRole <- QRole.findByName req.name
  whenJust mbExistingRole $ \_ -> throwError (RoleNameExists req.name)
  role <- buildRole req
  Esq.runTransaction $
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
  EsqDBFlow m r =>
  TokenInfo ->
  Id DRole.Role ->
  AssignAccessLevelReq ->
  m APISuccess
assignAccessLevel _ roleId req = do
  _role <- QRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  mbAccessMatrixItem <- QMatrix.findByRoleIdAndEntity roleId req.apiEntity
  case mbAccessMatrixItem of
    Just accessMatrixItem -> do
      Esq.runTransaction $ do
        QMatrix.updateUserAccessType accessMatrixItem.id req.userAccessType
    Nothing -> do
      accessMatrixItem <- buildAccessMatrixItem roleId req
      Esq.runTransaction $ do
        QMatrix.create accessMatrixItem
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
        userAccessType = req.userAccessType,
        createdAt = now,
        updatedAt = now
      }
