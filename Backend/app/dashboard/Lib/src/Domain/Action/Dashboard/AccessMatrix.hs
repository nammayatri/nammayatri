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
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, logInfo)
import Storage.Beam.BeamFlow
import qualified Storage.Queries.AccessMatrix as QMatrix
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.Role as QRole
import Tools.Auth
import Tools.Error
import qualified Data.Text as T

getMerchantWithCityList ::
  BeamFlow m r =>
  m [DMatrix.MerchantCityList]
getMerchantWithCityList = do
  logInfo "[AccessMatrix.getMerchantWithCityList] START"
  merchantList <- B.runInReplica QMerchant.findAllMerchants
  logInfo $ "[AccessMatrix.getMerchantWithCityList] findAllMerchants done, count=" <> show (length merchantList)
  let merchantCityList = map (\merchant -> DMatrix.MerchantCityList merchant.shortId merchant.supportedOperatingCities (map (T.pack . show) merchant.supportedOperatingCities)) merchantList
  pure merchantCityList

getAccessMatrix ::
  BeamFlow m r =>
  TokenInfo ->
  Maybe Integer ->
  Maybe Integer ->
  m DMatrix.AccessMatrixAPIEntity
getAccessMatrix _ mbLimit mbOffset = do
  logInfo $ "[AccessMatrix.getAccessMatrix] START | limit=" <> show mbLimit <> " offset=" <> show mbOffset
  roles <- B.runInReplica $ QRole.findAllByLimitOffset mbLimit mbOffset
  logInfo $ "[AccessMatrix.getAccessMatrix] findAllByLimitOffset done, roles count=" <> show (length roles)
  accessMatrixItems <- B.runInReplica $ QMatrix.findAllByRoles roles
  logInfo $ "[AccessMatrix.getAccessMatrix] findAllByRoles done, items count=" <> show (length accessMatrixItems)
  pure $ DMatrix.mkAccessMatrixAPIEntity roles accessMatrixItems

getAccessMatrixByRole ::
  BeamFlow m r =>
  TokenInfo ->
  Id DRole.Role ->
  m DMatrix.AccessMatrixRowAPIEntity
getAccessMatrixByRole _ roleId = do
  logInfo $ "[AccessMatrix.getAccessMatrixByRole] START | roleId=" <> roleId.getId
  role <- B.runInReplica $ QRole.findById roleId >>= fromMaybeM (RoleDoesNotExist roleId.getId)
  logInfo "[AccessMatrix.getAccessMatrixByRole] findById done"
  accessMatrixItems <- B.runInReplica $ QMatrix.findAllByRoleId roleId
  logInfo $ "[AccessMatrix.getAccessMatrixByRole] findAllByRoleId done, items count=" <> show (length accessMatrixItems)
  pure $ DMatrix.mkAccessMatrixRowAPIEntity accessMatrixItems role
