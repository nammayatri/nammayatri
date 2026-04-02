module Domain.Action.Internal.OfferDiscount where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Lib.Payment.Domain.Types.Offer as DOffer
import qualified SharedLogic.Payment as SPayment
import Storage.Beam.Payment ()
import qualified Storage.Queries.Booking as QBooking

data OfferDiscountResp = OfferDiscountResp
  { discountAmount :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

getOfferDiscount ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Maybe Text ->
  Text ->
  Maybe HighPrecMoney ->
  m OfferDiscountResp
getOfferDiscount _token bppBookingId mbFareAmount = do
  booking <- B.runInReplica $ QBooking.findByBPPBookingId (Id bppBookingId) >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId)
  case (booking.selectedOfferId, mbFareAmount) of
    (Just offerId, Just fareAmount) -> do
      mbComputed <- SPayment.getOfferAmount (Id @DOffer.Offer offerId) fareAmount
      pure $ OfferDiscountResp {discountAmount = (.discountAmount) <$> mbComputed}
    _ -> pure $ OfferDiscountResp {discountAmount = booking.discountAmount}
