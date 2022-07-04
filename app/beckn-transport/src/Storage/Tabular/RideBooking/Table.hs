{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RideBooking.Table where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.RideBooking as Domain
import qualified Domain.Types.Vehicle as Veh
import Storage.Tabular.BookingLocation (BookingLocationTId)
import Storage.Tabular.FareProduct ()
import Storage.Tabular.Organization (OrganizationTId)
import Storage.Tabular.RiderDetails (RiderDetailsTId)
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
      startTime UTCTime
      riderId RiderDetailsTId Maybe
      fromLocationId BookingLocationTId
      toLocationId BookingLocationTId Maybe
      vehicleVariant Veh.Variant
      estimatedFare Amount
      discount Amount Maybe
      estimatedTotalFare Amount
      estimatedDistance Double Maybe
      reallocationsCount Int
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RideBookingT where
  type DomainKey RideBookingT = Id Domain.RideBooking
  fromKey (RideBookingTKey _id) = Id _id
  toKey (Id id) = RideBookingTKey id
