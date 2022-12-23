{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.BookingCancellationReason where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
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
      driverId PersonTId Maybe
      bookingId BookingTId
      rideId RideTId Maybe
      source Domain.CancellationSource
      reasonCode CancellationReasonTId Maybe
      additionalInfo Text Maybe
      Primary bookingId
      UniqueBookingCancellationReasonBookingId bookingId
      deriving Generic
    |]

instance TType BookingCancellationReasonT Domain.BookingCancellationReason where
  fromTType BookingCancellationReasonT {..} = do
    return $
      Domain.BookingCancellationReason
        { bookingId = fromKey bookingId,
          rideId = fromKey <$> rideId,
          reasonCode = fromKey <$> reasonCode,
          driverId = fromKey <$> driverId,
          ..
        }
  toTType Domain.BookingCancellationReason {..} =
    BookingCancellationReasonT
      { driverId = toKey <$> driverId,
        bookingId = toKey bookingId,
        rideId = toKey <$> rideId,
        reasonCode = toKey <$> reasonCode,
        ..
      }
