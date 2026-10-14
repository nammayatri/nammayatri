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
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import Tools.Error

-- | BAP half of the "find a better driver" trigger: checks what only the rider side can
-- see, then calls the BPP, which re-checks ride state and live driver proximity.
postRideBookingBetterDriverSearch :: Id DP.Person -> Id DRB.Booking -> Flow APISuccess
postRideBookingBetterDriverSearch personId bookingId = do
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  -- "Not yours" reported as "not found", per convention - avoids leaking existence.
  unless (booking.riderId == personId) $ throwError (BookingDoesNotExist bookingId.getId)
  unless (booking.status == DRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  riderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
  unless (riderConfig.betterDriverSearchEnabled == Just True) $ throwError (InvalidRequest "Better driver search is not enabled for this city")
  -- Enforced server-side, not just hidden in the app, so the rollout is a real gate.
  unless (booking.eligibleForBetterDriverSearch == Just True) $ throwError (InvalidRequest "This booking was not selected for the better-driver-search rollout")
  let tripDistanceMeters = fromMaybe 0 (distanceToMeters <$> booking.estimatedDistance)
      minDistance = fromMaybe 0 riderConfig.minTripDistanceForBetterDriverSearch
  unless (tripDistanceMeters >= minDistance) $
    throwError (InvalidRequest "Trip too short for a better-driver search")
  -- The BPP keeps its own booking id, so send that, not this app's.
  bppBookingId <- fromMaybeM (InvalidRequest "BPP booking id not available for this booking") booking.bppBookingId
  CallBPPInternal.betterDriverSearch merchant.driverOfferBaseUrl bppBookingId.getId
