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
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Beam.AccessMatrix as BeamAM
import Storage.Beam.BeamFlow

create :: BeamFlow m r => DMatrix.AccessMatrixItem -> m ()
create = createWithKV

findByRoleIdAndEntityAndActionType ::
  BeamFlow m r =>
  Id DRole.Role ->
  DMatrix.ApiEntity ->
  DMatrix.UserActionType ->
  m (Maybe DMatrix.AccessMatrixItem)
findByRoleIdAndEntityAndActionType roleId apiEntity userActionType =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamAM.roleId $ Se.Eq $ getId roleId,
          Se.Is BeamAM.apiEntity $ Se.Eq apiEntity,
          Se.Is BeamAM.userActionType $ Se.Eq userActionType
        ]
    ]

findAllByRoles ::
  BeamFlow m r =>
  [DRole.Role] ->
  m [DMatrix.AccessMatrixItem]
findAllByRoles roles = do
  let roleKeys = map (getId . (.id)) roles
  findAllWithKV [Se.Is BeamAM.roleId $ Se.In roleKeys]

findAllByRoleId ::
  BeamFlow m r =>
  Id DRole.Role ->
  m [DMatrix.AccessMatrixItem]
findAllByRoleId roleId = findAllWithKV [Se.Is BeamAM.roleId $ Se.Eq $ getId roleId]

updateUserAccessType ::
  BeamFlow m r =>
  Id DMatrix.AccessMatrixItem ->
  DMatrix.UserActionType ->
  DMatrix.UserAccessType ->
  m ()
updateUserAccessType accessMatrixItemId userActionType userAccessType = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamAM.userActionType userActionType,
      Se.Set BeamAM.userAccessType userAccessType,
      Se.Set BeamAM.updatedAt now
    ]
    [Se.Is BeamAM.id $ Se.Eq $ getId accessMatrixItemId]

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
