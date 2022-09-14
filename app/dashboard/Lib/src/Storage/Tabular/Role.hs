{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Role where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Role as Domain

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

instance TType RoleT Domain.Role where
  fromTType RoleT {..} = do
    return $
      Domain.Role
        { id = Id id,
          ..
        }
  toTType Domain.Role {..} =
    RoleT
      { id = getId id,
        ..
      }
