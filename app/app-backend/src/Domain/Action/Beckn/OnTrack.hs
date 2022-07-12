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
import qualified Storage.Queries.Ride as QRide
import Types.Error
import Utils.Common

data OnTrackReq = OnTrackReq
  { bppRideId :: Id BPPRide,
    trackUrl :: BaseUrl
  }

onTrack :: EsqDBFlow m r => OnTrackReq -> m ()
onTrack req = do
  ride <- QRide.findByBPPRideId req.bppRideId >>= fromMaybeM (RideBookingDoesNotExist $ "BppRideId:" <> req.bppRideId.getId)
  DB.runTransaction $ do
    QRide.updateTrackingUrl ride.id req.trackUrl
