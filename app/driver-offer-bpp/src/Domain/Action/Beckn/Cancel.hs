{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

--TODO: Do not forget to remove these ^^^^^

module Domain.Action.Beckn.Cancel
  ( cancel,
    CancelReq (..),
  )
where

import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Domain.Types.Organization as Org
import EulerHS.Prelude

newtype CancelReq = CancelReq
  { bookingId :: Id Text --SRB.RideBooking
  }

cancel ::
  (EsqDBFlow m r) =>
  Id Org.Organization ->
  SignatureAuthResult ->
  CancelReq ->
  m ()
cancel transporterId _ req = do
  -- transporterOrg <-
  --   Organization.findById transporterId
  --     >>= fromMaybeM (OrgNotFound transporterId.getId)
  -- booking <- QRB.findById req.bookingId >>= fromMaybeM (RideBookingDoesNotExist req.bookingId.getId)
  -- let transporterId' = booking.providerId
  -- unless (transporterId' == transporterId) $ throwError AccessDenied
  -- now <- getCurrentTime
  -- rideReq <- BP.buildRideReq (booking.id) (transporterOrg.shortId) SRideRequest.CANCELLATION now
  -- Esq.runTransaction $ RideRequest.create rideReq
  return ()
