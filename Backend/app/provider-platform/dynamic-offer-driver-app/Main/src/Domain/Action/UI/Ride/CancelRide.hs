{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.CancelRide
  ( CancelRideReq (..),
    ServiceHandle (..),
    cancelRideHandle,
    driverCancelRideHandler,
    dashboardCancelRideHandler,
  )
where

import qualified Domain.Action.UI.Ride.CancelRide.Internal as CInternal
import qualified Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.CancellationReason (CancellationReasonCode (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

type MonadHandler m = (MonadThrow m, Log m, MonadGuid m)

data ServiceHandle m = ServiceHandle
  { findRideById :: Id DRide.Ride -> m (Maybe DRide.Ride),
    findById :: Id DP.Person -> m (Maybe DP.Person),
    cancelRide :: Id DRide.Ride -> DBCR.BookingCancellationReason -> m ()
  }

cancelRideHandle :: ServiceHandle Flow
cancelRideHandle =
  ServiceHandle
    { findRideById = QRide.findById,
      findById = QPerson.findById,
      cancelRide = CInternal.cancelRideImpl
    }

data CancelRideReq = CancelRideReq
  { reasonCode :: CancellationReasonCode,
    additionalInfo :: Maybe Text
  }

data RequestorId = PersonRequestorId (Id DP.Person) | DashboardRequestorId (Id DM.Merchant)

driverCancelRideHandler :: MonadHandler m => ServiceHandle m -> Id DP.Person -> Id DRide.Ride -> CancelRideReq -> m APISuccess.APISuccess
driverCancelRideHandler shandle personId rideId req =
  withLogTag ("rideId-" <> rideId.getId) $
    cancelRideHandler shandle (PersonRequestorId personId) rideId req

dashboardCancelRideHandler :: MonadHandler m => ServiceHandle m -> Id DM.Merchant -> Id DRide.Ride -> CancelRideReq -> m APISuccess.APISuccess
dashboardCancelRideHandler shandle merchantId rideId req =
  withLogTag ("merchantId-" <> merchantId.getId) $
    cancelRideHandler shandle (DashboardRequestorId merchantId) rideId req

cancelRideHandler :: MonadHandler m => ServiceHandle m -> RequestorId -> Id DRide.Ride -> CancelRideReq -> m APISuccess.APISuccess
cancelRideHandler ServiceHandle {..} requestorId rideId req = withLogTag ("rideId-" <> rideId.getId) do
  ride <- findRideById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (isValidRide ride) $ throwError $ RideInvalidStatus "This ride cannot be canceled"
  let driverId = ride.driverId

  rideCancelationReason <- case requestorId of
    PersonRequestorId personId -> do
      authPerson <-
        findById personId
          >>= fromMaybeM (PersonNotFound personId.getId)
      case authPerson.role of
        DP.ADMIN -> do
          driver <- findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
          unless (authPerson.merchantId == driver.merchantId) $ throwError (RideDoesNotExist rideId.getId)
          logTagInfo "admin -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
          buildRideCancelationReason Nothing DBCR.ByMerchant ride
        DP.DRIVER -> do
          unless (authPerson.id == driverId) $ throwError NotAnExecutor
          logTagInfo "driver -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
          buildRideCancelationReason (Just driverId) DBCR.ByDriver ride
    DashboardRequestorId reqMerchantId -> do
      driver <- findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      unless (driver.merchantId == reqMerchantId) $ throwError (RideDoesNotExist rideId.getId)
      logTagInfo "dashboard -> cancelRide : " ("DriverId " <> getId driverId <> ", RideId " <> getId ride.id)
      buildRideCancelationReason Nothing DBCR.ByMerchant ride -- is it correct DBCR.ByMerchant?
  cancelRide rideId rideCancelationReason
  pure APISuccess.Success
  where
    isValidRide ride =
      ride.status == DRide.NEW
    buildRideCancelationReason mbDriverId source ride = do
      let CancelRideReq {..} = req
      return $
        DBCR.BookingCancellationReason
          { bookingId = ride.bookingId,
            rideId = Just ride.id,
            source = source,
            reasonCode = Just reasonCode,
            driverId = mbDriverId,
            ..
          }
