module Domain.Booking.API where

import Beckn.Prelude
import Beckn.Types.Amount
import Domain.Booking.Type
import Domain.FerryStation
import Domain.PaymentTransaction

data BookingAPIEntity = BookingAPIEntity
  { quantity :: Int,
    ferrySupportNumber :: Text,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStation :: FerryStationAPIEntity,
    arrivalStation :: FerryStationAPIEntity,
    status :: BookingStatus,
    paymentTxn :: Maybe PaymentTransactionAPIEntity,
    ticketId :: Maybe Text,
    ticketCreatedAt :: Maybe UTCTime,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, ToSchema)

makeBookingAPIEntity :: Booking -> FerryStation -> FerryStation -> Maybe PaymentTransaction -> BookingAPIEntity
makeBookingAPIEntity Booking {..} departureStation arrivalStation mbPaymentTxn =
  BookingAPIEntity
    { departureStation = makeFerryStationAPIEntity departureStation,
      arrivalStation = makeFerryStationAPIEntity arrivalStation,
      paymentTxn = makePaymentTransactionAPIEntity <$> mbPaymentTxn,
      ..
    }

newtype BookingListRes = BookingListRes
  { list :: [BookingAPIEntity]
  }
  deriving (Generic, ToJSON, ToSchema)
