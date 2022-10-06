module Domain.Action.UI.Booking
  ( BookingListRes (..),
    bookingStatus,
    bookingList,
  )
where

import Beckn.Types.Id
import Beckn.Utils.Common
import Data.OpenApi (ToSchema (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Booking as QRB
import Tools.Error

newtype BookingListRes = BookingListRes
  { list :: [SRB.BookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

bookingStatus :: EsqDBFlow m r => Id SRB.Booking -> Id Person.Person -> m SRB.BookingAPIEntity
bookingStatus bookingId personId = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.riderId == personId) $ throwError AccessDenied
  SRB.buildBookingAPIEntity booking

bookingList :: EsqDBFlow m r => Id Person.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m BookingListRes
bookingList personId mbLimit mbOffset mbOnlyActive = do
  rbList <- QRB.findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive
  BookingListRes <$> traverse SRB.buildBookingAPIEntity rbList
