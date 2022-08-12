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
import qualified Storage.Queries.Merchant as QMerch
import Types.Error
import Utils.Common

newtype OnConfirmReq = OnConfirmReq
  { bppBookingId :: Id DRB.BPPBooking
  }

onConfirm :: EsqDBFlow m r => BaseUrl -> OnConfirmReq -> m ()
onConfirm registryUrl req = do
  booking <- QRB.findByBPPBookingId req.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId" <> req.bppBookingId.getId)

  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findByRegistryUrl registryUrl
  unless (elem booking.merchantId $ merchant <&> (.id)) $ throwError (InvalidRequest "No merchant which works with passed registry.")

  DB.runTransaction $ do
    QRB.updateStatus booking.id DRB.CONFIRMED
