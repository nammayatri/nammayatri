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

module Storage.Tabular.Rating where

import qualified Domain.Types.Rating as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
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

instance FromTType RatingT Domain.Rating where
  fromTType RatingT {..} = do
    return $
      Domain.Rating
        { id = Id id,
          driverId = fromKey driverId,
          rideId = fromKey rideId,
          ..
        }

instance ToTType RatingT Domain.Rating where
  toTType Domain.Rating {..} =
    RatingT
      { id = getId id,
        driverId = toKey driverId,
        rideId = toKey rideId,
        ..
      }
