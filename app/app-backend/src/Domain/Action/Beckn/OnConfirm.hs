module Domain.Action.Beckn.OnConfirm
  ( onConfirm,
    OnConfirmReq (..),
  )
where

import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.Booking as DRB
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Booking as QRB
import Types.Error
import Utils.Common

newtype OnConfirmReq = OnConfirmReq
  { bppBookingId :: Id DRB.BPPBooking
  }

onConfirm :: EsqDBFlow m r => OnConfirmReq -> m ()
onConfirm req = do
  booking <- QRB.findByBPPBookingId req.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId" <> req.bppBookingId.getId)
  DB.runTransaction $ do
    QRB.updateStatus booking.id DRB.CONFIRMED
