{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.BookingCancellationReason where

import qualified Domain.Types.BookingCancellationReason as Domain
import qualified Domain.Types.CancellationReason as DCR
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import qualified Storage.Tabular.Booking as SRB
import qualified Storage.Tabular.CancellationReason as SCR
import qualified Storage.Tabular.Ride as SRide

derivePersistField "Domain.CancellationSource"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingCancellationReasonT sql=booking_cancellation_reason
      bookingId SRB.BookingTId
      rideId SRide.RideTId Maybe
      source Domain.CancellationSource
      reasonCode SCR.CancellationReasonTId Maybe
      reasonStage DCR.CancellationStage Maybe
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
          ..
        }
  toTType Domain.BookingCancellationReason {..} =
    BookingCancellationReasonT
      { bookingId = toKey bookingId,
        rideId = toKey <$> rideId,
        reasonCode = toKey <$> reasonCode,
        ..
      }
