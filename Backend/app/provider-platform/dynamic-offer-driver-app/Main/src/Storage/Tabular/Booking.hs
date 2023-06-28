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

module Storage.Tabular.Booking where

import qualified Domain.Types.Booking as Domain
import qualified Domain.Types.Vehicle.Variant as Veh
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import qualified Storage.Tabular.FareParameters as Fare
import qualified Storage.Tabular.FareParameters.Instances as Fare
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.RiderDetails (RiderDetailsTId)
import Storage.Tabular.Vehicle ()

derivePersistField "Domain.BookingStatus"
derivePersistField "Domain.BookingType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingT sql=booking
      id Text
      transactionId Text
      quoteId Text
      status Domain.BookingStatus
      bookingType Domain.BookingType
      specialZoneOtpCode Text Maybe
      providerId MerchantTId
      primaryExophone Text
      bapId Text
      bapUri Text
      startTime UTCTime
      riderId RiderDetailsTId Maybe
      vehicleVariant Veh.Variant
      estimatedDistance Meters
      maxEstimatedDistance Centesimal Maybe
      estimatedFare Money
      estimatedDuration Seconds
      fareParametersId Fare.FareParametersTId
      riderName Text Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey BookingT where
  type DomainKey BookingT = Id Domain.Booking
  fromKey (BookingTKey _id) = Id _id
  toKey (Id id) = BookingTKey id

type FullBookingT = (BookingT, Fare.FullFareParametersT)

instance FromTType BookingT Domain.BookingTable where
  fromTType BookingT {..} = do
    pUrl <- parseBaseUrl bapUri
    return $
      Domain.BookingTable
        { id = Id id,
          providerId = fromKey providerId,
          bapUri = pUrl,
          maxEstimatedDistance = HighPrecMeters <$> maxEstimatedDistance,
          riderId = fromKey <$> riderId,
          fareParametersId = fromKey fareParametersId,
          ..
        }

instance ToTType BookingT Domain.BookingTable where
  toTType Domain.BookingTable {..} =
    BookingT
      { id = getId id,
        providerId = toKey providerId,
        bapUri = showBaseUrl bapUri,
        riderId = toKey <$> riderId,
        maxEstimatedDistance = getHighPrecMeters <$> maxEstimatedDistance,
        fareParametersId = toKey fareParametersId,
        ..
      }
