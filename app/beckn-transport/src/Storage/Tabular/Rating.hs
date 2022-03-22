{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Rating where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Rating as Domain
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.Ride (RideTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RatingT sql=rating
      id Text
      rideId RideTId
      driverId PersonTId
      ratingValue Int
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RatingT where
  type DomainKey RatingT = Id Domain.Rating
  fromKey (RatingTKey _id) = Id _id
  toKey (Id id) = RatingTKey id

instance TEntity RatingT Domain.Rating where
  fromTEntity entity = do
    let RatingT {..} = entityVal entity
    return $
      Domain.Rating
        { id = Id id,
          driverId = fromKey driverId,
          rideId = fromKey rideId,
          ..
        }
  toTType Domain.Rating {..} =
    RatingT
      { id = getId id,
        driverId = toKey driverId,
        rideId = toKey rideId,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
