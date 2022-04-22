{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SavedReqLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.SavedReqLocation as Domain
import qualified Storage.Tabular.Person as Person

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SavedReqLocationT sql=saved_location
      id Text
      lat Double
      lon Double
      street Text Maybe
      door Text Maybe
      city Text Maybe
      state Text Maybe
      country Text Maybe
      building Text Maybe
      areaCode Text Maybe
      area Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      tag  Text
      riderId Person.PersonTId
      Primary id
      deriving Generic
    |]

instance TEntityKey SavedReqLocationT where
  type DomainKey SavedReqLocationT = Id Domain.SavedReqLocation
  fromKey (SavedReqLocationTKey _id) = Id _id
  toKey (Id id) = SavedReqLocationTKey id

instance TEntity SavedReqLocationT Domain.SavedReqLocation where
  fromTEntity entity = do
    let SavedReqLocationT {..} = entityVal entity
    return $
      Domain.SavedReqLocation
        { id = Id id,
          riderId = fromKey riderId,
          ..
        }
  toTType Domain.SavedReqLocation {..} =
    SavedReqLocationT
      { id = getId id,
        riderId = toKey riderId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
