{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RideBookingCancellationReason where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.RideBookingCancellationReason as Domain
import Storage.Tabular.CancellationReason (CancellationReasonTId)
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.Ride (RideTId)
import Storage.Tabular.RideBooking (RideBookingTId)

derivePersistField "Domain.CancellationSource"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideBookingCancellationReasonT sql=ride_booking_cancellation_reason
      id Text
      driverId PersonTId Maybe
      rideBookingId RideBookingTId
      rideId RideTId Maybe
      source Domain.CancellationSource
      reasonCode CancellationReasonTId Maybe
      additionalInfo Text Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey RideBookingCancellationReasonT where
  type DomainKey RideBookingCancellationReasonT = Id Domain.RideBookingCancellationReason
  fromKey (RideBookingCancellationReasonTKey _id) = Id _id
  toKey (Id id) = RideBookingCancellationReasonTKey id

instance TType RideBookingCancellationReasonT Domain.RideBookingCancellationReason where
  fromTType RideBookingCancellationReasonT {..} = do
    return $
      Domain.RideBookingCancellationReason
        { id = Id id,
          driverId = fromKey <$> driverId,
          rideBookingId = fromKey rideBookingId,
          rideId = fromKey <$> rideId,
          reasonCode = fromKey <$> reasonCode,
          ..
        }
  toTType Domain.RideBookingCancellationReason {..} =
    RideBookingCancellationReasonT
      { id = getId id,
        driverId = toKey <$> driverId,
        rideBookingId = toKey rideBookingId,
        rideId = toKey <$> rideId,
        reasonCode = toKey <$> reasonCode,
        ..
      }
