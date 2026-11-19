{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The cancellation reason that marks a booking retired by a reschedule. Kept apart
-- from "SharedLogic.Reschedule" so the cancel callbacks can check it without
-- importing the reschedule flow (which itself depends on the cancel path).
module SharedLogic.RescheduleReason
  ( rescheduleCancellationReasonCode,
    isRescheduleCancellation,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as Fields
import qualified Domain.Types.CancellationReason as SCR
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.BookingCancellationReason as QBCR

-- | Reason code of the cancel that retires the old booking. Sent to the BPP as the
-- cancel descriptor's short_desc, which is how the BPP recognises a reschedule.
rescheduleCancellationReasonCode :: Text
rescheduleCancellationReasonCode = "RESCHEDULED"

-- | Whether the booking was cancelled to reschedule it. Such a cancel must not count
-- as a customer cancellation (fraud counters, cancellation rate, cancel tags).
isRescheduleCancellation :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DRB.Booking -> m Bool
isRescheduleCancellation bookingId = do
  mbReason <- QBCR.findByRideBookingId bookingId
  pure $ (mbReason >>= (.reasonCode)) == Just (SCR.CancellationReasonCode rescheduleCancellationReasonCode)
