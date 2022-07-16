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
import qualified Domain.Types.FareParams as Params
import qualified Domain.Types.RideBooking as Domain
import qualified Domain.Types.Vehicle.Variant as Veh
import Storage.Tabular.DriverQuote (DriverQuoteTId)
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.RideBooking.BookingLocation hiding (createdAt, id, updatedAt)
import Storage.Tabular.RiderDetails (RiderDetailsTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.RideBookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideBookingT sql=ride_booking
      id Text
      quoteId DriverQuoteTId
      status Domain.RideBookingStatus
      providerId OrganizationTId
      bapId Text
      bapUri Text
      startTime UTCTime
      riderId RiderDetailsTId Maybe
      fromLocationId BookingLocationTId
      toLocationId BookingLocationTId
      vehicleVariant Veh.Variant
      estimatedDistance Double
      createdAt UTCTime
      updatedAt UTCTime

      fareForPickup Amount
      distanceFare Amount
      driverSelectedFare Amount Maybe
      nightShiftRate Amount Maybe
      nightCoefIncluded Bool

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
        fareParams = Params.FareParameters {..}
    return $
      Domain.RideBooking
        { id = Id id,
          quoteId = fromKey quoteId,
          providerId = fromKey providerId,
          fromLocation = fromLoc_,
          toLocation = toLoc_,
          bapUri = pUrl,
          riderId = fromKey <$> riderId,
          estimatedDistance = HighPrecMeters estimatedDistance,
          ..
        }
  toTType Domain.RideBooking {..} =
    let Params.FareParameters {..} = fareParams
     in ( RideBookingT
            { id = getId id,
              quoteId = toKey quoteId,
              providerId = toKey providerId,
              fromLocationId = toKey fromLocation.id,
              toLocationId = toKey toLocation.id,
              bapUri = showBaseUrl bapUri,
              riderId = toKey <$> riderId,
              estimatedDistance = getHighPrecMeters estimatedDistance,
              ..
            },
          mkTabularBookingLocation fromLocation,
          mkTabularBookingLocation toLocation
        )
