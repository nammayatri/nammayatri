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

module Storage.Tabular.Feedback where

import qualified Domain.Types.Feedback as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.Ride.Table (RideTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FeedbackT sql=feedback
      id Text
      driverId PersonTId
      rideId RideTId
      badge Text
      Primary id
      deriving Generic
    |]

instance TEntityKey FeedbackT where
  type DomainKey FeedbackT = Id Domain.Feedback
  fromKey (FeedbackTKey _id) = Id _id
  toKey (Id id) = FeedbackTKey id

instance FromTType FeedbackT Domain.Feedback where
  fromTType FeedbackT {..} = do
    return $
      Domain.Feedback
        { id = Id id,
          driverId = fromKey driverId,
          rideId = fromKey rideId,
          ..
        }

instance ToTType FeedbackT Domain.Feedback where
  toTType Domain.Feedback {..} =
    FeedbackT
      { id = getId id,
        driverId = toKey driverId,
        rideId = toKey rideId,
        ..
      }
