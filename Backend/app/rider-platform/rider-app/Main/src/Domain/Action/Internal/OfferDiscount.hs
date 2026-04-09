module Domain.Action.Internal.OfferDiscount where

import Data.Maybe (listToMaybe)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Types (ServiceFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified SharedLogic.Offer as SOffer
import Storage.Beam.Payment ()
import qualified Storage.Queries.Booking as QBooking

data OfferDiscountResp = OfferDiscountResp
  { discountAmount :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

getOfferDiscount ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    ServiceFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Maybe Text ->
  Text ->
  Maybe HighPrecMoney ->
  m OfferDiscountResp
getOfferDiscount _token bppBookingId mbFareAmount = do
  booking <- B.runInReplica $ QBooking.findByBPPBookingId (Id bppBookingId) >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId)
  case (booking.selectedOfferId, mbFareAmount) of
    (Just offerId, Just fareAmount) -> do
      let productId = show booking.vehicleServiceTierType
          price = mkPrice (Just booking.estimatedTotalFare.currency) fareAmount
      productOffers <- SOffer.offerListWithBasket booking.merchantId booking.riderId booking.merchantOperatingCityId DOrder.RideHailing [(productId, price)]
      case snd <$> find (\(pid, _) -> pid == productId) productOffers of
        Nothing -> throwError $ InternalError "No product offer found"
        Just resp -> do
          let filteredOffers = filter (\o -> o.offerId == offerId) resp.offerResp
          case listToMaybe filteredOffers of
            Nothing -> throwError $ InternalError "No filtered offer found"
            Just offer -> pure $ OfferDiscountResp {discountAmount = Just offer.discountAmount}
    _ -> throwError $ InternalError "No offer found"
