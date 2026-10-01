module Domain.Action.Internal.BookingLocations
  ( BookingLocationsRes (..),
    getBookingLocations,
  )
where

import Domain.Action.UI.Location (makeLocationAPIEntity)
import Domain.Types.Booking (Booking)
import Domain.Types.Location (LocationAPIEntity)
import Environment
import Kernel.Beam.Functions (runInReplica)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QBooking

data BookingLocationsRes = BookingLocationsRes
  { from :: LocationAPIEntity,
    to :: Maybe LocationAPIEntity,
    stops :: [LocationAPIEntity]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

getBookingLocations :: Id Booking -> Maybe Text -> Flow (Maybe BookingLocationsRes)
getBookingLocations bookingId apiKey = do
  mbBooking <- runInReplica $ QBooking.findById bookingId
  case mbBooking of
    Nothing -> pure Nothing
    Just booking -> do
      merchant <- QM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
      unless (Just merchant.internalApiKey == apiKey) $
        throwError $ AuthBlocked "Invalid BPP internal api key"
      pure . Just $
        BookingLocationsRes
          { from = makeLocationAPIEntity booking.fromLocation,
            to = makeLocationAPIEntity <$> booking.toLocation,
            stops = makeLocationAPIEntity <$> booking.stops
          }
