{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.AccessMatrix where

import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Role as DRole
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Beam.AccessMatrix as BeamAM
import Storage.Tabular.AccessMatrix

create :: DMatrix.AccessMatrixItem -> SqlDB ()
create = Esq.create

findByRoleIdAndEntityAndActionType ::
  (Transactionable m) =>
  Id DRole.Role ->
  DMatrix.ApiEntity ->
  DMatrix.UserActionType ->
  m (Maybe DMatrix.AccessMatrixItem)
findByRoleIdAndEntityAndActionType roleId apiEntity userActionType = findOne $ do
  accessMatrix <- from $ table @AccessMatrixT
  where_ $
    accessMatrix ^. AccessMatrixRoleId ==. val (toKey roleId)
      &&. accessMatrix ^. AccessMatrixApiEntity ==. val apiEntity
      &&. accessMatrix ^. AccessMatrixUserActionType ==. val userActionType
  return accessMatrix

findAllByRoles ::
  Transactionable m =>
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
  Transactionable m =>
  Id DRole.Role ->
  m [DMatrix.AccessMatrixItem]
findAllByRoleId roleId = do
  Esq.findAll $ do
    accessMatrix <- from $ table @AccessMatrixT
    where_ $
      accessMatrix ^. AccessMatrixRoleId ==. val (toKey roleId)
    return accessMatrix

updateUserAccessType :: Id DMatrix.AccessMatrixItem -> DMatrix.UserActionType -> DMatrix.UserAccessType -> SqlDB ()
updateUserAccessType accessMatrixItemId userActionType userAccessType = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ AccessMatrixUserActionType =. val userActionType,
        AccessMatrixUserAccessType =. val userAccessType,
        AccessMatrixUpdatedAt =. val now
      ]
    where_ $ tbl ^. AccessMatrixTId ==. val (toKey accessMatrixItemId)

instance FromTType' BeamAM.AccessMatrix DMatrix.AccessMatrixItem where
  fromTType' BeamAM.AccessMatrixT {..} = do
    return $
      Just
        DMatrix.AccessMatrixItem
          { id = Id id,
            roleId = Id roleId,
            ..
          }

instance ToTType' BeamAM.AccessMatrix DMatrix.AccessMatrixItem where
  toTType' DMatrix.AccessMatrixItem {..} =
    BeamAM.AccessMatrixT
      { id = getId id,
        roleId = getId roleId,
        ..
      }
