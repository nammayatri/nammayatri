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
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.RideBooking as Domain
import qualified Domain.Types.Vehicle.Variant as Veh
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.RideBooking.BookingLocation hiding (createdAt, id, updatedAt)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.RideBookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideBookingT sql=ride_booking
      id Text
      status Domain.RideBookingStatus
      providerId OrganizationTId
      bapId Text
      bapUri Text
      fromLocationId BookingLocationTId
      toLocationId BookingLocationTId
      vehicleVariant Veh.Variant
      estimatedFare Amount
      estimatedDistance Double
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RideBookingT where
  type DomainKey RideBookingT = Id Domain.RideBooking
  fromKey (RideBookingTKey _id) = Id _id
  toKey (Id id) = RideBookingTKey id

instance TType (RideBookingT, BookingLocationT, BookingLocationT) Domain.RideBooking where
  fromTType (RideBookingT {..}, fromLoc, toLoc) = do
    pUrl <- parseBaseUrl bapUri
    let fromLoc_ = mkDomainBookingLocation fromLoc
        toLoc_ = mkDomainBookingLocation toLoc
    return $
      Domain.RideBooking
        { id = Id id,
          providerId = fromKey providerId,
          fromLocation = fromLoc_,
          toLocation = toLoc_,
          bapUri = pUrl,
          estimatedDistance = HighPrecMeters estimatedDistance,
          ..
        }
  toTType Domain.RideBooking {..} =
    ( RideBookingT
        { id = getId id,
          providerId = toKey providerId,
          fromLocationId = toKey fromLocation.id,
          toLocationId = toKey toLocation.id,
          bapUri = showBaseUrl bapUri,
          estimatedDistance = getHighPrecMeters estimatedDistance,
          ..
        },
      mkTabularBookingLocation fromLocation,
      mkTabularBookingLocation toLocation
    )
