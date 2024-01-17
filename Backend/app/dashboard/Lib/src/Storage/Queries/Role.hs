{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Role where

import Domain.Types.Role as Role
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Beam.Role as BeamR
import Storage.Tabular.Role

create :: Role -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Role ->
  m (Maybe Role)
findById = Esq.findById

findByName ::
  Transactionable m =>
  Text ->
  m (Maybe Role)
findByName name = findOne $ do
  role <- from $ table @RoleT
  where_ $
    role ^. RoleName ==. val name
  return role

findByDashboardAccessType ::
  Transactionable m =>
  Role.DashboardAccessType ->
  m (Maybe Role)
findByDashboardAccessType dashboardAccessType =
  findOne $ do
    role <- from $ table @RoleT
    where_ $
      role ^. RoleDashboardAccessType ==. val dashboardAccessType
    return role

findAllByLimitOffset ::
  Transactionable m =>
  Maybe Integer ->
  Maybe Integer ->
  m [Role]
findAllByLimitOffset mbLimit mbOffset = do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  Esq.findAll $ do
    role <- from $ table @RoleT
    orderBy [asc $ role ^. RoleName]
    limit limitVal
    offset offsetVal
    pure role

findAllWithLimitOffset ::
  Transactionable m =>
  Maybe Integer ->
  Maybe Integer ->
  Maybe Text ->
  m [Role]
findAllWithLimitOffset mbLimit mbOffset mbSearchString = do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  Esq.findAll $ do
    role <- from $ table @RoleT
    where_ $
      Esq.whenJust_ mbSearchString (filterBySearchString role)
    orderBy [asc $ role ^. RoleName]
    limit limitVal
    offset offsetVal
    pure role
  where
    filterBySearchString role searchStr = do
      let likeSearchStr = (%) ++. val searchStr ++. (%)
      role ^. RoleName
        `ilike` likeSearchStr

instance FromTType' BeamR.Role Role.Role where
  fromTType' BeamR.RoleT {..} = do
    return $
      Just
        Role.Role
          { id = Id id,
            ..
          }

instance ToTType' BeamR.Role Role.Role where
  toTType' Role.Role {..} =
    BeamR.RoleT
      { id = getId id,
        ..
      }
