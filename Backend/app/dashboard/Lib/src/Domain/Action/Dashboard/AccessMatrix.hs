{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.AccessMatrix where

import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import qualified Storage.Queries.AccessMatrix as QMatrix
import qualified Storage.Queries.Role as QRole
import Tools.Auth
import Tools.Error

getAccessMatrix ::
  forall m r.
  EsqDBReplicaFlow m r =>
  TokenInfo ->
  Maybe Integer ->
  Maybe Integer ->
  m DMatrix.AccessMatrixAPIEntity
getAccessMatrix _ mbLimit mbOffset = do
  roles <- runInReplica $ QRole.findAllByLimitOffset mbLimit mbOffset (Proxy @m)
  accessMatrixItems <- runInReplica $ QMatrix.findAllByRoles roles (Proxy @m)
  pure $ DMatrix.mkAccessMatrixAPIEntity roles accessMatrixItems

getAccessMatrixByRole ::
  forall m r.
  EsqDBReplicaFlow m r =>
  TokenInfo ->
  Id DRole.Role ->
  m DMatrix.AccessMatrixRowAPIEntity
getAccessMatrixByRole _ roleId = do
  role <- runInReplica $ QRole.findById roleId (Proxy @m) >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  accessMatrixItems <- runInReplica $ QMatrix.findAllByRoleId roleId (Proxy @m)
  pure $ DMatrix.mkAccessMatrixRowAPIEntity accessMatrixItems role
