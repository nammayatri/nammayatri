{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.AccessMatrix where

import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.AccessMatrix

create :: DMatrix.AccessMatrixItem -> SqlDB ()
create = Esq.create

findByRoleIdAndEntity ::
  (Transactionable m) =>
  Id DRole.Role ->
  DMatrix.ApiEntity ->
  m (Maybe DMatrix.AccessMatrixItem)
findByRoleIdAndEntity roleId apiEntity = findOne $ do
  accessMatrix <- from $ table @AccessMatrixT
  where_ $
    accessMatrix ^. AccessMatrixRoleId ==. val (toKey roleId)
      &&. accessMatrix ^. AccessMatrixApiEntity ==. val apiEntity
  return accessMatrix

findAllByRoles ::
  Transactionable ma m =>
  [DRole.Role] ->
  m [DMatrix.AccessMatrixItem]
findAllByRoles roles = do
  let roleKeys = map (toKey . (.id)) roles
  Esq.findAll $ do
    accessMatrix <- from $ table @AccessMatrixT
    where_ $
      accessMatrix ^. AccessMatrixRoleId `in_` valList roleKeys
    return accessMatrix

findAllByRoleId ::
  Transactionable ma m =>
  Id DRole.Role ->
  m [DMatrix.AccessMatrixItem]
findAllByRoleId roleId = do
  Esq.findAll $ do
    accessMatrix <- from $ table @AccessMatrixT
    where_ $
      accessMatrix ^. AccessMatrixRoleId ==. val (toKey roleId)
    return accessMatrix

updateUserAccessType :: Id DMatrix.AccessMatrixItem -> DMatrix.UserAccessType -> SqlDB ()
updateUserAccessType accessMatrixItemId userAccessType = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ AccessMatrixUserAccessType =. val userAccessType,
        AccessMatrixUpdatedAt =. val now
      ]
    where_ $ tbl ^. AccessMatrixTId ==. val (toKey accessMatrixItemId)
