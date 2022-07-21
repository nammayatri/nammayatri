module Domain.Action.Beckn.OnInit where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.Booking (BPPBooking, Booking)
import qualified Storage.Queries.Booking as QRideB
import Utils.Common

data OnInitReq = OnInitReq
  { bookingId :: Id Booking,
    bppBookingId :: Id BPPBooking,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

onInit :: EsqDBFlow m r => OnInitReq -> m ()
onInit req = do
  DB.runTransaction $ do
    QRideB.updateBPPBookingId req.bookingId req.bppBookingId
    QRideB.updatePaymentInfo req.bookingId req.estimatedFare req.discount req.estimatedTotalFare
