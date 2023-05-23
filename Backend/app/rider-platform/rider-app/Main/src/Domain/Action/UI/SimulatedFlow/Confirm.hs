module Domain.Action.UI.SimulatedFlow.Confirm where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Quote as Quote
import Kernel.Prelude hiding (init)
import Kernel.Types.Id
import Kernel.Utils.Common
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
