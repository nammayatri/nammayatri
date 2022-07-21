{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.BookingCancellationReason where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.BookingCancellationReason as Domain
import Storage.Tabular.Booking (BookingTId)
import Storage.Tabular.CancellationReason (CancellationReasonTId)
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.Ride (RideTId)

derivePersistField "Domain.CancellationSource"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingCancellationReasonT sql=booking_cancellation_reason
      id Text
      driverId PersonTId Maybe
      bookingId BookingTId
      rideId RideTId Maybe
      source Domain.CancellationSource
      reasonCode CancellationReasonTId Maybe
      additionalInfo Text Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey BookingCancellationReasonT where
  type DomainKey BookingCancellationReasonT = Id Domain.BookingCancellationReason
  fromKey (BookingCancellationReasonTKey _id) = Id _id
  toKey (Id id) = BookingCancellationReasonTKey id

instance TType BookingCancellationReasonT Domain.BookingCancellationReason where
  fromTType BookingCancellationReasonT {..} = do
    return $
      Domain.BookingCancellationReason
        { id = Id id,
          bookingId = fromKey bookingId,
          rideId = fromKey <$> rideId,
          reasonCode = fromKey <$> reasonCode,
          driverId = fromKey <$> driverId,
          ..
        }
  toTType Domain.BookingCancellationReason {..} =
    BookingCancellationReasonT
      { id = getId id,
        driverId = toKey <$> driverId,
        bookingId = toKey bookingId,
        rideId = toKey <$> rideId,
        reasonCode = toKey <$> reasonCode,
        ..
      }
