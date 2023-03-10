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

module Storage.Tabular.Role where

import qualified Domain.Types.Role as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id

derivePersistField "Domain.DashboardAccessType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RoleT sql=role
      id Text
      name Text
      dashboardAccessType Domain.DashboardAccessType
      description Text
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RoleT where
  type DomainKey RoleT = Id Domain.Role
  fromKey (RoleTKey _id) = Id _id
  toKey (Id id) = RoleTKey id

instance FromTType RoleT Domain.Role where
  fromTType RoleT {..} = do
    return $
      Domain.Role
        { id = Id id,
          ..
        }

instance ToTType RoleT Domain.Role where
  toTType Domain.Role {..} =
    RoleT
      { id = getId id,
        ..
      }
