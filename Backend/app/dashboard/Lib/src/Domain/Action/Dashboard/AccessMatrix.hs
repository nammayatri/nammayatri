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
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.Role as QRole
import Tools.Auth
import Tools.Error

getMerchantWithCityList ::
  EsqDBReplicaFlow m r =>
  m [DMatrix.MerchantCityList]
getMerchantWithCityList = do
  merchantList <- runInReplica QMerchant.findAllMerchants
  let merchantCityList = map (\merchant -> DMatrix.MerchantCityList merchant.shortId merchant.supportedOperatingCities) merchantList
  pure merchantCityList

getAccessMatrix ::
  EsqDBReplicaFlow m r =>
  TokenInfo ->
  Maybe Integer ->
  Maybe Integer ->
  m DMatrix.AccessMatrixAPIEntity
getAccessMatrix _ mbLimit mbOffset = do
  roles <- runInReplica $ QRole.findAllByLimitOffset mbLimit mbOffset
  accessMatrixItems <- runInReplica $ QMatrix.findAllByRoles roles
  pure $ DMatrix.mkAccessMatrixAPIEntity roles accessMatrixItems

getAccessMatrixByRole ::
  EsqDBReplicaFlow m r =>
  TokenInfo ->
  Id DRole.Role ->
  m DMatrix.AccessMatrixRowAPIEntity
getAccessMatrixByRole _ roleId = do
  role <- runInReplica $ QRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  accessMatrixItems <- runInReplica $ QMatrix.findAllByRoleId roleId
  pure $ DMatrix.mkAccessMatrixRowAPIEntity accessMatrixItems role
