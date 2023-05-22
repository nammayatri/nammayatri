module SharedLogic.SimulatedFlow.Confirm where

import Kernel.Prelude hiding (init)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Types.Booking.Type as DRB
import qualified SharedLogic.Types.Quote as Quote
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.SimulatedFlow.SearchRequest as CSR

newtype ConfirmRes = ConfirmRes
  { bookingId :: Id DRB.Booking
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

simulateBooking :: (MonadFlow m, SimluatedCacheFlow m r) => Id Quote.Quote -> m ConfirmRes
simulateBooking quoteId = do
  guid <- generateGUID
  CSR.linkBookingToQuoteId quoteId guid
  return $
    ConfirmRes
      { bookingId = guid
      }
