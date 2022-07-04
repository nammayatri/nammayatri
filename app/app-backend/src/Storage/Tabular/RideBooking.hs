{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RideBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Common (HighPrecMeters (..))
import Beckn.Types.Id
import qualified Domain.Types.RideBooking as Domain
import qualified Domain.Types.VehicleVariant as VehVar (VehicleVariant)
import qualified Storage.Tabular.BookingLocation as SLoc
import qualified Storage.Tabular.Person as SPerson

derivePersistField "Domain.RideBookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideBookingT sql=ride_booking
      id Text
      bppBookingId Text Maybe sql=bpp_ride_booking_id
      status Domain.RideBookingStatus
      providerId Text
      providerUrl Text
      providerName Text
      providerMobileNumber Text
      startTime UTCTime
      riderId SPerson.PersonTId
      fromLocationId SLoc.BookingLocationTId
      toLocationId SLoc.BookingLocationTId Maybe
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      distance Double Maybe
      vehicleVariant VehVar.VehicleVariant
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RideBookingT where
  type DomainKey RideBookingT = Id Domain.RideBooking
  fromKey (RideBookingTKey _id) = Id _id
  toKey (Id id) = RideBookingTKey id

instance TType RideBookingT Domain.RideBooking where
  fromTType RideBookingT {..} = do
    pUrl <- parseBaseUrl providerUrl
    return $
      Domain.RideBooking
        { id = Id id,
          bppBookingId = Id <$> bppBookingId,
          riderId = fromKey riderId,
          fromLocationId = fromKey fromLocationId,
          toLocationId = fromKey <$> toLocationId,
          providerUrl = pUrl,
          distance = HighPrecMeters <$> distance,
          ..
        }
  toTType Domain.RideBooking {..} =
    RideBookingT
      { id = getId id,
        bppBookingId = getId <$> bppBookingId,
        riderId = toKey riderId,
        fromLocationId = toKey fromLocationId,
        toLocationId = toKey <$> toLocationId,
        providerUrl = showBaseUrl providerUrl,
        distance = getHighPrecMeters <$> distance,
        ..
      }
