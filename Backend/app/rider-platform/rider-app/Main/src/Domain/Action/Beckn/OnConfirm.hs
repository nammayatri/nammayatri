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
    validateRequest,
    OnConfirmReq (..),
  )
where

import qualified Domain.Types.Booking as DRB
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import Tools.Error

data OnConfirmReq = OnConfirmReq
  { bppBookingId :: Id DRB.BPPBooking,
    specialZoneOtp :: Maybe Text
  }

data ValidatedOnConfirmReq = ValidatedOnConfirmReq
  { bppBookingId :: Id DRB.BPPBooking,
    specialZoneOtp :: Maybe Text,
    booking :: DRB.Booking
  }

onConfirm :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => ValidatedOnConfirmReq -> m ()
onConfirm ValidatedOnConfirmReq {..} = do
  whenJust specialZoneOtp $ \otp ->
    -- DB.runTransaction $ do
    void $ QRB.updateOtpCodeBookingId booking.id otp
  -- DB.runTransaction $ do
  void $ QRB.updateStatus booking.id DRB.CONFIRMED

validateRequest :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => OnConfirmReq -> m ValidatedOnConfirmReq
validateRequest OnConfirmReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId" <> bppBookingId.getId)
  -- booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId" <> bppBookingId.getId)
  return $ ValidatedOnConfirmReq {..}
