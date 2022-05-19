module Domain.Action.Beckn.OnInit where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.RideBooking (BPPRideBooking, RideBooking)
import qualified Storage.Queries.RideBooking as QRideB
import Utils.Common

data OnInitReq = OnInitReq
  { rideBookingId :: Id RideBooking,
    bppRideBookingId :: Id BPPRideBooking,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

onInit :: EsqDBFlow m r => OnInitReq -> m ()
onInit req = do
  DB.runTransaction $ do
    QRideB.updateBPPBookingId req.rideBookingId req.bppRideBookingId
    QRideB.updatePaymentInfo req.rideBookingId req.estimatedFare req.discount req.estimatedTotalFare
