module Domain.Action.Beckn.OnConfirm
  ( onConfirm,
    OnConfirmReq (..),
  )
where

import qualified Domain.Types.Booking as DRB
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.Booking as QRB
import Tools.Error

newtype OnConfirmReq = OnConfirmReq
  { bppBookingId :: Id DRB.BPPBooking
  }

onConfirm :: (CacheFlow m r, EsqDBFlow m r) => BaseUrl -> OnConfirmReq -> m ()
onConfirm registryUrl req = do
  booking <- QRB.findByBPPBookingId req.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId" <> req.bppBookingId.getId)

  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

  DB.runTransaction $ do
    QRB.updateStatus booking.id DRB.CONFIRMED
