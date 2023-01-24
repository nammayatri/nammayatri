{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.AccessMatrix where

import qualified Domain.Types.AccessMatrix as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Role (RoleTId)

derivePersistField "Domain.UserAccessType"
derivePersistField "Domain.ApiEntity"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    AccessMatrixT sql=access_matrix
      id Text
      roleId RoleTId
      apiEntity Domain.ApiEntity
      userAccessType Domain.UserAccessType
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey AccessMatrixT where
  type DomainKey AccessMatrixT = Id Domain.AccessMatrixItem
  fromKey (AccessMatrixTKey _id) = Id _id
  toKey (Id id) = AccessMatrixTKey id

instance FromTType AccessMatrixT Domain.AccessMatrixItem where
  fromTType AccessMatrixT {..} = do
    return $
      Domain.AccessMatrixItem
        { id = Id id,
          roleId = fromKey roleId,
          ..
        }

instance ToTType AccessMatrixT Domain.AccessMatrixItem where
  toTType Domain.AccessMatrixItem {..} =
    AccessMatrixT
      { id = getId id,
        roleId = toKey roleId,
        ..
      }
