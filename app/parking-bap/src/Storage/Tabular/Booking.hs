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
import qualified Domain.Booking as Domain
import Storage.Tabular.Quote (QuoteTId)
import Storage.Tabular.Search (SearchTId)

derivePersistField "Domain.BookingStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    BookingT sql=booking
      id Text
      searchId SearchTId
      quoteId QuoteTId
      requestorId Text
      requestorNumber Text
      vehicleNumber Text
      bppId Text
      bppUrl Text
      bppItemId Text
      parkingSpaceName Text
      parkingSpaceLocationId Text
      fare Amount
      fromDate UTCTime
      toDate UTCTime
      status Domain.BookingStatus
      ticketId Text Maybe
      ticketCreatedAt UTCTime Maybe
      updatedAt UTCTime
      createdAt UTCTime
      bppOrderId Text Maybe
      requestorName Text
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
          ..
        }
  toTType Domain.Booking {..} = do
    BookingT
      { id = id.getId,
        searchId = toKey searchId,
        quoteId = toKey quoteId,
        requestorId = requestorId.getId,
        bppUrl = showBaseUrl bppUrl,
        ..
      }
  toTEntity a = do
    Entity (toKey a.id) $ toTType a
