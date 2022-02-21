module Domain.Types.Booking.API where

import Beckn.Prelude
import Beckn.Types.Amount
import Domain.Types.Booking.Type
import Domain.Types.PaymentTransaction
import Domain.Types.TransportStation

data BookingAPIEntity = BookingAPIEntity
  { quantity :: Int,
    publicTransportSupportNumber :: Text,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStation :: TransportStationAPIEntity,
    arrivalStation :: TransportStationAPIEntity,
    status :: BookingStatus,
    paymentTxn :: Maybe PaymentTransactionAPIEntity,
    ticketId :: Maybe Text,
    ticketCreatedAt :: Maybe UTCTime,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

makeBookingAPIEntity :: Booking -> TransportStation -> TransportStation -> Maybe PaymentTransaction -> BookingAPIEntity
makeBookingAPIEntity Booking {..} departureStation arrivalStation mbPaymentTxn =
  BookingAPIEntity
    { departureStation = makeTransportStationAPIEntity departureStation,
      arrivalStation = makeTransportStationAPIEntity arrivalStation,
      paymentTxn = makePaymentTransactionAPIEntity <$> mbPaymentTxn,
      ..
    }

newtype BookingListRes = BookingListRes
  { list :: [BookingAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
