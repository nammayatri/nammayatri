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

create :: DMatrix.AccessMatrixItem -> SqlDB m ()
create = Esq.create

findByRoleIdAndEntity ::
  forall m ma.
  Transactionable ma m =>
  Id DRole.Role ->
  DMatrix.ApiEntity ->
  Proxy ma ->
  m (Maybe DMatrix.AccessMatrixItem)
findByRoleIdAndEntity roleId apiEntity _ = findOne @m @ma $ do
  accessMatrix <- from $ table @AccessMatrixT
  where_ $
    accessMatrix ^. AccessMatrixRoleId ==. val (toKey roleId)
      &&. accessMatrix ^. AccessMatrixApiEntity ==. val apiEntity
  return accessMatrix

findAllByRoles ::
  forall m ma.
  Transactionable ma m =>
  [DRole.Role] ->
  Proxy ma ->
  m [DMatrix.AccessMatrixItem]
findAllByRoles roles _ = do
  let roleKeys = map (toKey . (.id)) roles
  Esq.findAll @m @ma $ do
    accessMatrix <- from $ table @AccessMatrixT
    where_ $
      accessMatrix ^. AccessMatrixRoleId `in_` valList roleKeys
    return accessMatrix

findAllByRoleId ::
  forall m ma.
  Transactionable ma m =>
  Id DRole.Role ->
  Proxy ma ->
  m [DMatrix.AccessMatrixItem]
findAllByRoleId roleId _ = do
  Esq.findAll @m @ma $ do
    accessMatrix <- from $ table @AccessMatrixT
    where_ $
      accessMatrix ^. AccessMatrixRoleId ==. val (toKey roleId)
    return accessMatrix

updateUserAccessType :: Id DMatrix.AccessMatrixItem -> DMatrix.UserAccessType -> SqlDB m ()
updateUserAccessType accessMatrixItemId userAccessType = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ AccessMatrixUserAccessType =. val userAccessType,
        AccessMatrixUpdatedAt =. val now
      ]
    where_ $ tbl ^. AccessMatrixTId ==. val (toKey accessMatrixItemId)
