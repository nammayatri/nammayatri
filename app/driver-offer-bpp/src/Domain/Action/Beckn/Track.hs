{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

--TODO: Do not forget to remove these ^^^^^

module Domain.Action.Beckn.Track
  ( track,
    DTrackReq (..),
    DTrackRes (..),
  )
where

import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.Organization as Org
import EulerHS.Prelude

newtype DTrackReq = TrackReq
  { rideId :: Id Text --DRide.Ride
  }

data DTrackRes = TrackRes
  { url :: BaseUrl,
    transporter :: Org.Organization
  }

track ::
  (EsqDBFlow m r) =>
  Id Org.Organization ->
  DTrackReq ->
  m DTrackRes
track transporterId req = do
  -- transporter <-
  --   QOrg.findById transporterId
  --     >>= fromMaybeM (OrgNotFound transporterId.getId)
  -- ride <- QRide.findById req.rideId >>= fromMaybeM (RideDoesNotExist req.rideId.getId)
  -- booking <- QRB.findById ride.bookingId >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)
  -- let transporterId' = booking.providerId
  -- unless (transporterId' == transporterId) $ throwError AccessDenied
  return $
    TrackRes
      { ..
      }
