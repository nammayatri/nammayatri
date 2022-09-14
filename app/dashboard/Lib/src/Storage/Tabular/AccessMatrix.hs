{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.AccessMatrix where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.AccessMatrix as Domain
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
  type DomainKey AccessMatrixT = Id Domain.AccessMatrix
  fromKey (AccessMatrixTKey _id) = Id _id
  toKey (Id id) = AccessMatrixTKey id

instance TType AccessMatrixT Domain.AccessMatrix where
  fromTType AccessMatrixT {..} = do
    return $
      Domain.AccessMatrix
        { id = Id id,
          roleId = fromKey roleId,
          ..
        }
  toTType Domain.AccessMatrix {..} =
    AccessMatrixT
      { id = getId id,
        roleId = toKey roleId,
        ..
      }
