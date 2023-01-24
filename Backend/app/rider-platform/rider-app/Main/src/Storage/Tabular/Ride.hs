{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Ride where

import qualified Domain.Types.Ride as Domain
import qualified Domain.Types.VehicleVariant as VehVar (VehicleVariant)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Centesimal, HighPrecMeters, HighPrecMoney)
import Kernel.Types.Id
import qualified Storage.Tabular.Booking as SRB

derivePersistField "Domain.RideStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideT sql=ride
      id Text
      bppRideId Text
      bookingId SRB.BookingTId
      shortId Text
      status Domain.RideStatus
      driverName Text
      driverRating Centesimal Maybe
      driverMobileNumber Text
      driverRegisteredAt UTCTime
      vehicleNumber Text
      vehicleModel Text
      vehicleColor Text
      vehicleVariant VehVar.VehicleVariant
      otp Text
      trackingUrl Text Maybe
      fare HighPrecMoney Maybe
      totalFare HighPrecMoney Maybe
      chargeableDistance HighPrecMeters Maybe
      driverArrivalTime UTCTime Maybe
      rideStartTime UTCTime Maybe
      rideEndTime UTCTime Maybe
      rideRating Int Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      Unique RideShortId
      deriving Generic
    |]

instance TEntityKey RideT where
  type DomainKey RideT = Id Domain.Ride
  fromKey (RideTKey _id) = Id _id
  toKey (Id id) = RideTKey id

instance FromTType RideT Domain.Ride where
  fromTType RideT {..} = do
    tUrl <- parseBaseUrl `mapM` trackingUrl
    return $
      Domain.Ride
        { id = Id id,
          bppRideId = Id bppRideId,
          bookingId = fromKey bookingId,
          shortId = ShortId shortId,
          trackingUrl = tUrl,
          fare = roundToIntegral <$> fare,
          totalFare = roundToIntegral <$> totalFare,
          ..
        }

instance ToTType RideT Domain.Ride where
  toTType Domain.Ride {..} =
    RideT
      { id = getId id,
        bppRideId = getId bppRideId,
        bookingId = toKey bookingId,
        shortId = getShortId shortId,
        trackingUrl = showBaseUrl <$> trackingUrl,
        fare = realToFrac <$> fare,
        totalFare = realToFrac <$> totalFare,
        ..
      }
