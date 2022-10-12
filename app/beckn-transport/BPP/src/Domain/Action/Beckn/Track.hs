module Domain.Action.Beckn.Track
  ( track,
    DTrackReq (..),
    DTrackRes (..),
  )
where

import Beckn.Storage.Hedis
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude
import qualified Storage.CachedQueries.Organization as QOrg
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error

newtype DTrackReq = TrackReq
  { rideId :: Id DRide.Ride
  }

data DTrackRes = TrackRes
  { url :: BaseUrl,
    transporter :: Org.Organization
  }

track ::
  (HedisFlow m r, EsqDBFlow m r) =>
  Id Org.Organization ->
  DTrackReq ->
  m DTrackRes
track transporterId req = do
  transporter <-
    QOrg.findById transporterId
      >>= fromMaybeM (OrgNotFound transporterId.getId)
  ride <- QRide.findById req.rideId >>= fromMaybeM (RideDoesNotExist req.rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let transporterId' = booking.providerId
  unless (transporterId' == transporterId) $ throwError AccessDenied
  return $
    TrackRes
      { url = ride.trackingUrl,
        ..
      }
