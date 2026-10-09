{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.BetterDriverSearch
  ( postRideBookingBetterDriverSearch,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingStatus as DRB
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Distance (distanceToMeters)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import Tools.Error

-- | Rider-side half of the "find a better driver" trigger: checks everything only
-- the BAP can see (RiderConfig's enabled flag and minimum trip distance, against this
-- rider-app's own copy of the booking) before ever calling the BPP. The BPP
-- (Domain.Action.Internal.BetterDriverSearch on the driver-app side) re-checks
-- everything it alone knows - the booking/ride state and live driver proximity - and
-- only then actually starts the stand-by search.
--
-- Deliberately not enforced here yet: a one-attempt-per-booking limit. Left out for
-- now by explicit choice - revisit before this ships.
postRideBookingBetterDriverSearch :: Id DP.Person -> Id DRB.Booking -> Flow APISuccess
postRideBookingBetterDriverSearch personId bookingId = do
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  -- Treat "not yours" the same as "not found" (no dedicated access-denied error
  -- exists in this codebase for resource-ownership checks; this is the established
  -- convention instead - avoids leaking whether the booking exists at all).
  unless (booking.riderId == personId) $ throwError (BookingDoesNotExist bookingId.getId)
  unless (booking.status == DRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
  unless (riderConfig.betterDriverSearchEnabled == Just True) $ throwError (InvalidRequest "Better driver search is not enabled for this city")
  -- Rollout gate: eligibleForBetterDriverSearch was rolled once, at ride-assignment
  -- time, against RiderConfig.betterDriverSearchEligibilityProbability (see
  -- Domain.Action.Beckn.Common.assignRideUpdate) and never re-rolled since. Enforced
  -- here (not just hidden in the app UI) so the rollout percentage is a real gate,
  -- not just a display hint a rider could bypass by calling this endpoint directly.
  unless (booking.eligibleForBetterDriverSearch == Just True) $ throwError (InvalidRequest "This booking was not selected for the better-driver-search rollout")
  let tripDistanceMeters = fromMaybe 0 (distanceToMeters <$> booking.estimatedDistance)
      minDistance = fromMaybe 0 riderConfig.minTripDistanceForBetterDriverSearch
  unless (tripDistanceMeters >= minDistance) $
    throwError (InvalidRequest "Trip too short for a better-driver search")
  CallBPPInternal.betterDriverSearch merchant.driverOfferBaseUrl booking.id.getId
