{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.Booking as Domain
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
      fare Amount
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

instance TEntity BookingT Domain.Booking where
  fromTEntity entity = do
    let BookingT {..} = entityVal entity
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
          ..
        }
  toTType Domain.Booking {..} = do
    BookingT
      { id = id.getId,
        searchId = toKey searchId,
        quoteId = toKey quoteId,
        requestorId = requestorId.getId,
        bppUrl = showBaseUrl bppUrl,
        departureStationId = toKey departureStationId,
        arrivalStationId = toKey arrivalStationId,
        ..
      }
  toTEntity a = do
    Entity (toKey a.id) $ toTType a
