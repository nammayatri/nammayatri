{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The driver's side of a shared-route run: acknowledging the run they were given
-- (SharedLogic.SharedRun.Acknowledgement; idempotent, and the fallback job leaves an acknowledged run with its driver),
-- and reading it before or during the drive: whether it is acknowledged, and its stops and passengers in the shared
-- cab's session shape (SharedLogic.SharedRun.Session.runSession, the same session the driver gets during the run).
module Domain.Action.UI.SharedRun (postSharedRunAcknowledge, getSharedRunDetails) where

import qualified API.Types.UI.SharedRun as API
import qualified Domain.Types as DTrip
import qualified Domain.Types.Booking as Fields
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.SharedRun.Acknowledgement as Ack
import qualified SharedLogic.SharedRun.Session as RunSession
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide

postSharedRunAcknowledge :: (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id DRide.Ride -> Environment.Flow APISuccess
postSharedRunAcknowledge (mbPersonId, _, _) rideId = do
  (personId, ride) <- ownRun mbPersonId rideId
  unless (ride.status == DRide.UPCOMING) $ throwError (InvalidRequest "Only an upcoming run can be acknowledged")
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  Ack.acknowledgeRun ride.id booking.startTime
  logInfo $ "shared run " <> booking.id.getId <> ": driver " <> personId.getId <> " acknowledged ride " <> ride.id.getId
  pure Success

-- | One of the caller's own runs, upcoming or under way: acknowledged or not, and its route, stops and passengers.
getSharedRunDetails :: (Maybe (Id DP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Id DRide.Ride -> Environment.Flow API.SharedRunDetailsRes
getSharedRunDetails (mbPersonId, _, _) rideId = do
  (_, ride) <- ownRun mbPersonId rideId
  unless (ride.status `elem` [DRide.UPCOMING, DRide.INPROGRESS]) $ throwError (InvalidRequest "Only an upcoming or ongoing run has details")
  acknowledged <- Ack.isRunAcknowledged ride.id
  session <- RunSession.runSession ride
  pure API.SharedRunDetailsRes {acknowledged, session}

-- | The ride, when it is a shared-route run assigned to the caller.
ownRun :: Maybe (Id DP.Person) -> Id DRide.Ride -> Environment.Flow (Id DP.Person, DRide.Ride)
ownRun mbPersonId rideId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.driverId == personId) $ throwError (InvalidRequest "This run is not assigned to you")
  unless (DTrip.isSharedRouteTrip ride.tripCategory) $ throwError (InvalidRequest "This ride is not a shared run")
  pure (personId, ride)
