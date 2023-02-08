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

instance TType AccessMatrixT Domain.AccessMatrixItem where
  fromTType AccessMatrixT {..} = do
    return $
      Domain.AccessMatrixItem
        { id = Id id,
          roleId = fromKey roleId,
          ..
        }
  toTType Domain.AccessMatrixItem {..} =
    AccessMatrixT
      { id = getId id,
        roleId = toKey roleId,
        ..
      }
