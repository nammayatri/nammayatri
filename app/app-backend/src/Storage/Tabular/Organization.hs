{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Organization where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Organization as Domain

derivePersistField "Domain.OrganizationType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OrganizationT sql=organization
      id Text
      shortId Text
      orgType Domain.OrganizationType sql=type
      Primary id
      Unique OrganizationShortId
      deriving Generic
    |]

instance TEntityKey OrganizationT where
  type DomainKey OrganizationT = Id Domain.Organization
  fromKey (OrganizationTKey _id) = Id _id
  toKey (Id id) = OrganizationTKey id

instance TEntity OrganizationT Domain.Organization where
  fromTEntity entity = do
    let OrganizationT {..} = entityVal entity
    return $
      Domain.Organization
        { id = Id id,
          shortId = ShortId shortId,
          _type = orgType,
          ..
        }
  toTType Domain.Organization {..} =
    OrganizationT
      { id = getId id,
        shortId = getShortId shortId,
        orgType = _type,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
