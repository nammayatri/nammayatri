{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Person where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Person as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PersonT sql=person
      id Text
      name Text
      phone Text
      Primary id
      deriving Generic
    |]

instance TEntityKey PersonT where
  type DomainKey PersonT = Id Domain.Person
  fromKey (PersonTKey _id) = Id _id
  toKey id = PersonTKey id.getId

instance TEntity PersonT Domain.Person where
  fromTEntity entity = do
    let PersonT {..} = entityVal entity
    return $
      Domain.Person
        { id = Id id,
          ..
        }
  toTType Domain.Person {..} =
    PersonT
      { id = id.getId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
