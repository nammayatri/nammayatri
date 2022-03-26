{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Organization where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Organization as Domain
import Storage.Tabular.DunzoCreds (DunzoCredsTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OrganizationT sql=organization
      id Text
      shortId Text
      createdAt UTCTime
      updatedAt UTCTime
      dunzoCredsId DunzoCredsTId
      Primary id
      deriving Generic
    |]

instance TEntityKey OrganizationT where
  type DomainKey OrganizationT = Id Domain.Organization
  fromKey (OrganizationTKey _id) = Id _id
  toKey id = OrganizationTKey id.getId

instance TEntity OrganizationT Domain.Organization where
  fromTEntity entity = do
    let OrganizationT {..} = entityVal entity
    return $
      Domain.Organization
        { id = Id id,
          shortId = ShortId shortId,
          dunzoCredsId = fromKey dunzoCredsId,
          ..
        }
  toTType Domain.Organization {..} =
    OrganizationT
      { id = id.getId,
        shortId = getShortId shortId,
        dunzoCredsId = toKey dunzoCredsId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
