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
import Storage.Tabular.Ride.Table (RideTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RatingT sql=rating
      id Text
      rideId RideTId
      driverId PersonTId
      ratingValue Int
      feedbackDetails Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RatingT where
  type DomainKey RatingT = Id Domain.Rating
  fromKey (RatingTKey _id) = Id _id
  toKey (Id id) = RatingTKey id

instance TType RatingT Domain.Rating where
  fromTType RatingT {..} = do
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
