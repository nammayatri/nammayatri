module Domain.Action.Beckn.Cancel
  ( cancel,
    CancelReq (..),
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Domain.Types.Organization as Organization
import qualified Domain.Types.RideBooking as SRB
import qualified Domain.Types.RideRequest as SRideRequest
import EulerHS.Prelude
import qualified Product.BecknProvider.BP as BP
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RideRequest as RideRequest
import Types.Error
import Utils.Common

newtype CancelReq = CancelReq
  { bookingId :: Id SRB.RideBooking
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
  rideBooking <- QRB.findById req.bookingId >>= fromMaybeM (RideBookingDoesNotExist req.bookingId.getId)
  now <- getCurrentTime
  rideReq <- BP.buildRideReq (rideBooking.id) (transporterOrg.shortId) SRideRequest.CANCELLATION now
  Esq.runTransaction $ RideRequest.create rideReq
