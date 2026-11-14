module Domain.Action.Internal.OfferDiscount where

import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.ServiceTierType as DVST
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Types (ServiceFlow)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified SharedLogic.InternalAuth as SIA
import qualified SharedLogic.Offer as SOffer
import Storage.Beam.Payment ()
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide

data OfferDiscountReq = OfferDiscountReq
  { fareAmount :: Maybe HighPrecMoney,
    fareBreakups :: Maybe [FB.BreakupLine],
    serviceTier :: Maybe DVST.ServiceTierType
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
    BeamFlow m r,
    HasFlowEnv m r '["internalAPIKey" ::: Text, "enforcingInternalAuth" ::: Bool]
  ) =>
  Maybe Text ->
  Text ->
  OfferDiscountReq ->
  m OfferDiscountResp
getOfferDiscount apiKey bppBookingId req = do
  internalAPIKey <- asks (.internalAPIKey)
  SIA.checkInternalKey "offerDiscount" [internalAPIKey] apiKey
  booking <- B.runInReplica $ QBooking.findByBPPBookingId (Id bppBookingId) >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId)
  let mbOfferBase = req.fareAmount <&> \fare -> maybe fare (\breakupLines -> if null breakupLines then fare else FB.discountableAmount fare breakupLines) req.fareBreakups
  case (booking.selectedOfferId, mbOfferBase) of
    (Just offerId, Just offerBase) -> do
      let serviceTier = fromMaybe booking.vehicleServiceTierType req.serviceTier
          offerBooking = booking {DRB.vehicleServiceTierType = serviceTier}
          productId = show serviceTier
          price = mkPrice (Just booking.estimatedTotalFare.currency) offerBase
      riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
      let enableRideHailingOffers = maybe False (.enableRideHailingOffers) riderConfig
      unless enableRideHailingOffers $ throwError $ InternalError "RideHailing offers disabled"
      mbRide <- B.runInReplica $ QRide.findOneByBookingId booking.id
      productOffers <- SOffer.offerListWithBasket booking.merchantId booking.riderId booking.merchantOperatingCityId DOrder.RideHailing [(productId, price)] [] [] mbRide (Just offerBooking) Nothing
      let mbEligibleOffer = find (\(pid, _) -> pid == productId) productOffers >>= find (\o -> o.offerId == offerId) . (.offerResp) . snd
      pure $ OfferDiscountResp {discountAmount = (.discountAmount) <$> mbEligibleOffer}
    _ -> pure $ OfferDiscountResp {discountAmount = Nothing}
