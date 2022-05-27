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
import qualified Domain.Types.CancellationReason as DCR
import qualified Domain.Types.RideBookingCancellationReason as Domain
import qualified Storage.Tabular.CancellationReason as SCR
import qualified Storage.Tabular.Ride as SRide
import qualified Storage.Tabular.RideBooking as SRB

derivePersistField "Domain.CancellationSource"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideBookingCancellationReasonT sql=ride_booking_cancellation_reason
      id Text
      rideBookingId SRB.RideBookingTId
      rideId SRide.RideTId Maybe
      source Domain.CancellationSource
      reasonCode SCR.CancellationReasonTId Maybe
      reasonStage DCR.CancellationStage Maybe
      additionalInfo Text Maybe
      Primary id
      deriving Generic
    |]

instance TEntityKey RideBookingCancellationReasonT where
  type DomainKey RideBookingCancellationReasonT = Id Domain.RideBookingCancellationReason
  fromKey (RideBookingCancellationReasonTKey _id) = Id _id
  toKey (Id id) = RideBookingCancellationReasonTKey id

instance TEntity RideBookingCancellationReasonT Domain.RideBookingCancellationReason where
  fromTEntity entity = do
    let RideBookingCancellationReasonT {..} = entityVal entity
    return $
      Domain.RideBookingCancellationReason
        { id = Id id,
          rideBookingId = fromKey rideBookingId,
          rideId = fromKey <$> rideId,
          reasonCode = fromKey <$> reasonCode,
          ..
        }
  toTType Domain.RideBookingCancellationReason {..} =
    RideBookingCancellationReasonT
      { id = getId id,
        rideBookingId = toKey rideBookingId,
        rideId = toKey <$> rideId,
        reasonCode = toKey <$> reasonCode,
        ..
      }
  toTEntity a =
    Entity (toKey a.id) $ toTType a
