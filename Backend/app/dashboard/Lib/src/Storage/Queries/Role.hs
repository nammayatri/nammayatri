{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Role where

import Domain.Types.Role as Role
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Role

create :: Role -> SqlDB m ()
create = Esq.create

findById ::
  forall m ma.
  Transactionable ma m =>
  Id Role ->
  Proxy ma ->
  m (Maybe Role)
findById roleId _ = Esq.findById @m @ma roleId

findByName ::
  forall m ma.
  Transactionable ma m =>
  Text ->
  Proxy ma ->
  m (Maybe Role)
findByName name _ = findOne @m @ma $ do
  role <- from $ table @RoleT
  where_ $
    role ^. RoleName ==. val name
  return role

findAllByLimitOffset ::
  forall m ma.
  Transactionable ma m =>
  Maybe Integer ->
  Maybe Integer ->
  Proxy ma ->
  m [Role]
findAllByLimitOffset mbLimit mbOffset _ = do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  Esq.findAll @m @ma $ do
    role <- from $ table @RoleT
    orderBy [asc $ role ^. RoleName]
    limit limitVal
    offset offsetVal
    pure role

findAllWithLimitOffset ::
  forall m ma.
  Transactionable ma m =>
  Maybe Integer ->
  Maybe Integer ->
  Maybe Text ->
  Proxy ma ->
  m [Role]
findAllWithLimitOffset mbLimit mbOffset mbSearchString _ = do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  Esq.findAll @m @ma $ do
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
