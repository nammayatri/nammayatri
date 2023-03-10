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

module Storage.Tabular.BusinessEvent where

import qualified Domain.Types.BusinessEvent as Domain
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Storage.Tabular.Booking (BookingTId)
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.Ride (RideTId)

derivePersistField "Domain.EventType"
derivePersistField "Domain.WhenPoolWasComputed"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BusinessEventT sql=business_event
      id Text
      driverId PersonTId Maybe
      eventType Domain.EventType
      timeStamp UTCTime
      bookingId BookingTId Maybe
      whenPoolWasComputed Domain.WhenPoolWasComputed Maybe
      vehicleVariant Variant Maybe
      distance Int Maybe
      duration Int Maybe
      rideId RideTId Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey BusinessEventT where
  type DomainKey BusinessEventT = Id Domain.BusinessEvent
  fromKey (BusinessEventTKey _id) = Id _id
  toKey (Id id) = BusinessEventTKey id

instance FromTType BusinessEventT Domain.BusinessEvent where
  fromTType BusinessEventT {..} = do
    return $
      Domain.BusinessEvent
        { id = Id id,
          driverId = cast . fromKey <$> driverId,
          bookingId = fromKey <$> bookingId,
          rideId = fromKey <$> rideId,
          distance = Meters <$> distance,
          duration = Seconds <$> duration,
          ..
        }

instance ToTType BusinessEventT Domain.BusinessEvent where
  toTType Domain.BusinessEvent {..} =
    BusinessEventT
      { id = getId id,
        driverId = toKey . cast <$> driverId,
        bookingId = toKey <$> bookingId,
        rideId = toKey <$> rideId,
        distance = getMeters <$> distance,
        duration = getSeconds <$> duration,
        ..
      }
