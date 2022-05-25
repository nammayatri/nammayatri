module Domain.Action.Beckn.OnConfirm
  ( onConfirm,
    OnConfirmReq (..),
  )
where

import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.RideBooking as DRB
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.RideBooking as QRB
import Types.Error
import Utils.Common

newtype OnConfirmReq = OnConfirmReq
  { bppBookingId :: Id DRB.BPPRideBooking
  }

onConfirm :: EsqDBFlow m r => OnConfirmReq -> m ()
onConfirm req = do
  booking <- QRB.findByBPPBookingId req.bppBookingId >>= fromMaybeM RideBookingDoesNotExist
  DB.runTransaction $ do
    QRB.updateStatus booking.id DRB.CONFIRMED
