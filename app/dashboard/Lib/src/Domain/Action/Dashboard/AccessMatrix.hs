module Domain.Action.Dashboard.AccessMatrix where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Types.Id
import Beckn.Utils.Common (fromMaybeM)
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Role as DRole
import qualified Storage.Queries.AccessMatrix as QMatrix
import qualified Storage.Queries.Role as QRole
import Tools.Auth
import Tools.Error

getAccessMatrix ::
  EsqDBReplicaFlow m r =>
  TokenInfo ->
  Maybe Integer ->
  Maybe Integer ->
  m DMatrix.AccessMatrixAPIEntity
getAccessMatrix _ mbLimit mbOffset = do
  roles <- QRole.findAllByLimitOffset mbLimit mbOffset
  accessMatrixItems <- QMatrix.findAllByRoles roles
  pure $ DMatrix.mkAccessMatrixAPIEntity roles accessMatrixItems

getAccessMatrixByRole ::
  EsqDBReplicaFlow m r =>
  TokenInfo ->
  Id DRole.Role ->
  m DMatrix.AccessMatrixRowAPIEntity
getAccessMatrixByRole _ roleId = do
  role <- QRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  accessMatrixItems <- QMatrix.findAllByRoleId roleId
  pure $ DMatrix.mkAccessMatrixRowAPIEntity accessMatrixItems role
