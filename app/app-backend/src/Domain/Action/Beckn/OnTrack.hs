module Domain.Action.Beckn.OnTrack
  ( onTrack,
    OnTrackReq (..),
  )
where

import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Domain.Types.Ride
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Merchant as QMerch
import qualified Storage.Queries.Ride as QRide
import Types.Error
import Utils.Common

data OnTrackReq = OnTrackReq
  { bppRideId :: Id BPPRide,
    trackUrl :: BaseUrl
  }

onTrack :: EsqDBFlow m r => BaseUrl -> OnTrackReq -> m ()
onTrack registryUrl req = do
  ride <- QRide.findByBPPRideId req.bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId:" <> req.bppRideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)

  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findByRegistryUrl registryUrl
  unless (elem booking.merchantId $ merchant <&> (.id)) $ throwError (InvalidRequest "No merchant which works with passed registry.")

  DB.runTransaction $ do
    QRide.updateTrackingUrl ride.id req.trackUrl
