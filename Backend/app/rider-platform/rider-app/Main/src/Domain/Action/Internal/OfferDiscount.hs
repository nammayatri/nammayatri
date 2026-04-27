module Domain.Action.Internal.OfferDiscount where

import Data.Maybe (listToMaybe)
import Data.OpenApi (ToSchema)
import qualified Domain.SharedLogic.RideDiscount as RD
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Types (ServiceFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified SharedLogic.Offer as SOffer
import Storage.Beam.Payment ()
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide

-- | Request body for the BPP → BAP internal offer-discount lookup.
--   BPP sends the post-EndRide eight-slot breakup ('projectFareParamsBreakup');
--   BAP applies the offer against the discount-applicable ride fare
--   inclusive of its VAT. 'fareAmount' is a legacy fallback.
data OfferDiscountReq = OfferDiscountReq
  { fareAmount :: Maybe HighPrecMoney,
    projectFareParamsBreakup :: Maybe RD.ProjectFareParamsBreakup
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

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
    EsqDBReplicaFlow m r,
    BeamFlow m r
  ) =>
  Maybe Text ->
  Text ->
  OfferDiscountReq ->
  m OfferDiscountResp
getOfferDiscount _token bppBookingId req = do
  booking <- B.runInReplica $ QBooking.findByBPPBookingId (Id bppBookingId) >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId)
  let mbOfferBase = case req.projectFareParamsBreakup of
        Just b -> Just (b.discountApplicableRideFareTaxExclusive + b.discountApplicableRideFareTax)
        Nothing -> req.fareAmount
  case (booking.selectedOfferId, mbOfferBase) of
    (Just offerId, Just offerBase) -> do
      let productId = show booking.vehicleServiceTierType
          price = mkPrice (Just booking.estimatedTotalFare.currency) offerBase
      riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId})
      let enableRideHailingOffers = maybe False (.enableRideHailingOffers) riderConfig
      unless enableRideHailingOffers $ throwError $ InternalError "RideHailing offers disabled"
      mbRide <- B.runInReplica $ QRide.findByRBId booking.id
      productOffers <- SOffer.offerListWithBasket booking.merchantId booking.riderId booking.merchantOperatingCityId DOrder.RideHailing [(productId, price)] mbRide (Just booking) Nothing
      case snd <$> find (\(pid, _) -> pid == productId) productOffers of
        Nothing -> throwError $ InternalError "No product offer found"
        Just resp -> do
          let filteredOffers = filter (\o -> o.offerId == offerId) resp.offerResp
          case listToMaybe filteredOffers of
            Nothing -> throwError $ InternalError "No filtered offer found"
            Just offer -> pure $ OfferDiscountResp {discountAmount = Just offer.discountAmount}
    _ -> throwError $ InternalError "No offer found"
