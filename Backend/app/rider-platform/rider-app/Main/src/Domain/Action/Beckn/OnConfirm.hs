{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

onConfirm :: forall m r. (CacheFlow m r, EsqDBFlow m r) => BaseUrl -> OnConfirmReq -> m ()
onConfirm registryUrl req = do
  booking <- QRB.findByBPPBookingId req.bppBookingId (Proxy @m) >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId" <> req.bppBookingId.getId)

  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

  DB.runTransaction $ do
    QRB.updateStatus @m booking.id DRB.CONFIRMED
