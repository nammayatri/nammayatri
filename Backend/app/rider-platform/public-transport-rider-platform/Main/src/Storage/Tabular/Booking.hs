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
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Storage.Tabular.Quote (QuoteTId)
import Storage.Tabular.Search (SearchTId)
import Storage.Tabular.TransportStation (TransportStationTId)

derivePersistField "Domain.BookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingT sql=booking
      id Text
      searchId SearchTId
      quoteId QuoteTId
      bknTxnId Text
      requestorId Text
      quantity Int
      bppId Text
      bppUrl Text
      publicTransportSupportNumber Text
      description Text
      fare HighPrecMoney
      departureTime UTCTime
      arrivalTime UTCTime
      departureStationId TransportStationTId
      arrivalStationId TransportStationTId
      status Domain.BookingStatus
      ticketId Text Maybe
      ticketCreatedAt UTCTime Maybe
      updatedAt UTCTime
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey BookingT where
  type DomainKey BookingT = Id Domain.Booking
  fromKey (BookingTKey _id) = Id _id
  toKey id = BookingTKey id.getId

instance FromTType BookingT Domain.Booking where
  fromTType BookingT {..} = do
    bppUrl_ <- parseBaseUrl bppUrl
    return $
      Domain.Booking
        { id = Id id,
          searchId = fromKey searchId,
          quoteId = fromKey quoteId,
          requestorId = Id requestorId,
          bppUrl = bppUrl_,
          departureStationId = fromKey departureStationId,
          arrivalStationId = fromKey arrivalStationId,
          fare = roundToIntegral fare,
          ..
        }

instance ToTType BookingT Domain.Booking where
  toTType Domain.Booking {..} = do
    BookingT
      { id = id.getId,
        searchId = toKey searchId,
        quoteId = toKey quoteId,
        requestorId = requestorId.getId,
        bppUrl = showBaseUrl bppUrl,
        departureStationId = toKey departureStationId,
        arrivalStationId = toKey arrivalStationId,
        fare = realToFrac fare,
        ..
      }
