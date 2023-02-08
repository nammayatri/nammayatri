module Domain.Action.Beckn.OnTrack
  ( onTrack,
    OnTrackReq (..),
  )
where

import Domain.Types.Ride
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data OnTrackReq = OnTrackReq
  { bppRideId :: Id BPPRide,
    trackUrl :: BaseUrl
  }

onTrack :: (CacheFlow m r, EsqDBFlow m r) => BaseUrl -> OnTrackReq -> m ()
onTrack registryUrl req = do
  ride <- QRide.findByBPPRideId req.bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId:" <> req.bppRideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)

  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

  DB.runTransaction $ do
    QRide.updateTrackingUrl ride.id req.trackUrl
