module Domain.Action.Beckn.Cancel
  ( cancel,
    CancelReq (..),
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.RideRequest as SRideRequest
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BP
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.RideRequest as RideRequest
import Types.Error
import Utils.Common

newtype CancelReq = CancelReq
  { bookingId :: Id SRB.Booking
  }

cancel ::
  (EsqDBFlow m r) =>
  Id Organization.Organization ->
  SignatureAuthResult ->
  CancelReq ->
  m ()
cancel transporterId _ req = do
  transporterOrg <-
    Organization.findById transporterId
      >>= fromMaybeM (OrgNotFound transporterId.getId)
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  let transporterId' = booking.providerId
  unless (transporterId' == transporterId) $ throwError AccessDenied
  now <- getCurrentTime
  rideReq <- BP.buildRideReq (booking.id) (transporterOrg.shortId) SRideRequest.CANCELLATION now
  Esq.runTransaction $ RideRequest.create rideReq
