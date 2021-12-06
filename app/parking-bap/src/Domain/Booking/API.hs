module Domain.Booking.API where

import Beckn.Prelude
import Beckn.Types.Amount
import Domain.Booking.Type
import Domain.ParkingLocation
import Domain.PaymentTransaction

data BookingAPIEntity = BookingAPIEntity
  { vehicleNumber :: Text,
    additionalInfo :: Text,
    parkingSpaceName :: Text,
    parkingSpaceLocation :: ParkingLocationAPIEntity,
    parkingSupportNumber :: Text,
    fare :: Amount,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    status :: BookingStatus,
    ticketId :: Maybe Text,
    ticketCreatedAt :: Maybe UTCTime,
    paymentTxn :: Maybe PaymentTransactionAPIEntity,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON)

makeBookingAPIEntity :: Booking -> ParkingLocation -> Maybe PaymentTransaction -> BookingAPIEntity
makeBookingAPIEntity Booking {..} searchLoc mbPaymentTxn =
  BookingAPIEntity
    { parkingSpaceLocation = makeParkingLocationAPIEntity searchLoc,
      paymentTxn = makePaymentTransactionAPIEntity <$> mbPaymentTxn,
      ..
    }
