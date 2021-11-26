{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Domain.Booking
  ( module Storage.Domain.Booking,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Amount
import Beckn.Types.App (BaseUrl)
import Beckn.Types.Id
import qualified Data.Text as T
import Data.Time (UTCTime)
import Servant.Client (parseBaseUrl, showBaseUrl)
import Storage.Domain.Quote
import Storage.Domain.Search
import Storage.Tabular.Booking
import Storage.Tabular.Booking as Reexport (BookingT)
import Storage.Tabular.Booking as Reexport hiding (BookingT (..))

data Booking = Booking
  { id :: Id Booking,
    searchId :: Id Search,
    quoteId :: Id Quote,
    requestorId :: Text,
    requestorNumber :: Text,
    vehicleNumber :: Text,
    additionalInfo :: Text,
    bppId :: Text,
    bppUrl :: BaseUrl,
    parkingSpaceName :: Text,
    parkingSpaceLocationId :: Text,
    parkingSupportNumber :: Text,
    fare :: Amount,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    status :: BookingStatus,
    ticketId :: Maybe Text,
    ticketCreatedAt :: Maybe UTCTime,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }

instance TEntityKey BookingT Booking where
  fromKey (BookingTKey _id) = Id _id
  toKey Booking {id} = BookingTKey id.getId

instance TEntity BookingT Booking where
  fromTEntity entity = do
    let id = fromKey $ entityKey entity
        BookingT {..} = entityVal entity
    bppUrl <- parseBaseUrl $ T.unpack bookingTBppUrl
    return $
      Booking
        { id = id,
          searchId = let (SearchTKey _id) = bookingTSearchId in Id _id,
          quoteId = let (QuoteTKey _id) = bookingTQuoteId in Id _id,
          requestorId = bookingTRequestorId,
          requestorNumber = bookingTRequestorNumber,
          vehicleNumber = bookingTVehicleNumber,
          additionalInfo = bookingTAdditionalInfo,
          bppId = bookingTBppId,
          bppUrl = bppUrl,
          parkingSpaceName = bookingTParkingSpaceName,
          parkingSpaceLocationId = bookingTParkingSpaceLocationId,
          parkingSupportNumber = bookingTParkingSupportNumber,
          fare = bookingTFare,
          fromDate = bookingTFromDate,
          toDate = bookingTToDate,
          status = bookingTStatus,
          ticketId = bookingTTicketId,
          ticketCreatedAt = bookingTTicketCreatedAt,
          updatedAt = bookingTUpdatedAt,
          createdAt = bookingTCreatedAt
        }
  toTType Booking {..} = do
    BookingT
      { bookingTSearchId = SearchTKey searchId.getId,
        bookingTQuoteId = QuoteTKey quoteId.getId,
        bookingTRequestorId = requestorId,
        bookingTRequestorNumber = requestorNumber,
        bookingTVehicleNumber = vehicleNumber,
        bookingTAdditionalInfo = additionalInfo,
        bookingTBppId = bppId,
        bookingTBppUrl = T.pack $ showBaseUrl bppUrl,
        bookingTParkingSpaceName = parkingSpaceName,
        bookingTParkingSpaceLocationId = parkingSpaceLocationId,
        bookingTParkingSupportNumber = parkingSupportNumber,
        bookingTFare = fare,
        bookingTFromDate = fromDate,
        bookingTToDate = toDate,
        bookingTStatus = status,
        bookingTTicketId = ticketId,
        bookingTTicketCreatedAt = ticketCreatedAt,
        bookingTUpdatedAt = updatedAt,
        bookingTCreatedAt = createdAt
      }
  toTEntity a = do
    Entity (toKey a) $ toTType a
