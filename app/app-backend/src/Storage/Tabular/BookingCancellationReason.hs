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
import qualified Domain.Types.CancellationReason as DCR
import qualified Storage.Tabular.Booking as SRB
import qualified Storage.Tabular.CancellationReason as SCR
import qualified Storage.Tabular.Ride as SRide

derivePersistField "Domain.CancellationSource"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingCancellationReasonT sql=booking_cancellation_reason
      id Text
      bookingId SRB.BookingTId
      rideId SRide.RideTId Maybe
      source Domain.CancellationSource
      reasonCode SCR.CancellationReasonTId Maybe
      reasonStage DCR.CancellationStage Maybe
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
          ..
        }
  toTType Domain.BookingCancellationReason {..} =
    BookingCancellationReasonT
      { id = getId id,
        bookingId = toKey bookingId,
        rideId = toKey <$> rideId,
        reasonCode = toKey <$> reasonCode,
        ..
      }
