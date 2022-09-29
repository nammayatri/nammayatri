{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking.BookingLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Booking.BookingLocation as Domain
import qualified Domain.Types.LocationAddress as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingLocationT sql=booking_location
      id Text
      lat Double
      lon Double
      street Text Maybe
      door Text Maybe
      city Text Maybe
      state Text Maybe
      country Text Maybe
      building Text Maybe
      areaCode Text Maybe
      area Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey BookingLocationT where
  type DomainKey BookingLocationT = Id Domain.BookingLocation
  fromKey (BookingLocationTKey _id) = Id _id
  toKey (Id id) = BookingLocationTKey id

instance TType BookingLocationT Domain.BookingLocation where
  fromTType BookingLocationT {..} = do
    let address = Domain.LocationAddress {..}
    return $
      Domain.BookingLocation
        { id = Id id,
          ..
        }
  toTType Domain.BookingLocation {..} = do
    let Domain.LocationAddress {..} = address
    BookingLocationT
      { id = getId id,
        ..
      }
