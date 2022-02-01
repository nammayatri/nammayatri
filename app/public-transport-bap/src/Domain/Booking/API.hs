module Domain.Booking.API where

import Beckn.Prelude
import Beckn.Types.Amount
import Domain.Booking.Type
import Domain.PaymentTransaction
import Domain.PublicTranport

data BookingAPIEntity = BookingAPIEntity
  { quantity :: Int,
    publicTransportSupportNumber :: Text,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStation :: PublicTranportAPIEntity,
    arrivalStation :: PublicTranportAPIEntity,
    status :: BookingStatus,
    paymentTxn :: Maybe PaymentTransactionAPIEntity,
    ticketId :: Maybe Text,
    ticketCreatedAt :: Maybe UTCTime,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, ToSchema)

makeBookingAPIEntity :: Booking -> PublicTranport -> PublicTranport -> Maybe PaymentTransaction -> BookingAPIEntity
makeBookingAPIEntity Booking {..} departureStation arrivalStation mbPaymentTxn =
  BookingAPIEntity
    { departureStation = makePublicTranportAPIEntity departureStation,
      arrivalStation = makePublicTranportAPIEntity arrivalStation,
      paymentTxn = makePaymentTransactionAPIEntity <$> mbPaymentTxn,
      ..
    }

newtype BookingListRes = BookingListRes
  { list :: [BookingAPIEntity]
  }
  deriving (Generic, ToJSON, ToSchema)
