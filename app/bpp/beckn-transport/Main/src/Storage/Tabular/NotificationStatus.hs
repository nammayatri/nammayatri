{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.NotificationStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.NotificationStatus as Domain
import Storage.Tabular.Booking (BookingTId)
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.AnswerStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    NotificationStatusT sql=notification_status
      id Text
      bookingId BookingTId
      driverId PersonTId
      status Domain.AnswerStatus
      expiresAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey NotificationStatusT where
  type DomainKey NotificationStatusT = Id Domain.NotificationStatus
  fromKey (NotificationStatusTKey _id) = Id _id
  toKey (Id id) = NotificationStatusTKey id

instance TType NotificationStatusT Domain.NotificationStatus where
  fromTType NotificationStatusT {..} = do
    return $
      Domain.NotificationStatus
        { id = Id id,
          driverId = cast $ fromKey driverId,
          bookingId = fromKey bookingId,
          ..
        }
  toTType Domain.NotificationStatus {..} =
    NotificationStatusT
      { id = getId id,
        driverId = toKey $ cast driverId,
        bookingId = toKey bookingId,
        ..
      }
