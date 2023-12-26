{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Action.Beckn.Status
  ( handler,
    DStatusReq (..),
    DStatusRes (..),
    OnStatusBuildReq (..),
  )
where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import Kernel.Beam.Functions as B
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.SyncRide as SyncRide
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide

newtype DStatusReq = StatusReq
  { bookingId :: Id DBooking.Booking
  }

data DStatusRes = DStatusRes
  { transporter :: DM.Merchant,
    info :: OnStatusBuildReq
  }

data OnStatusBuildReq
  = NewBookingBuildReq
      { bookingId :: Id DBooking.Booking
      }
  | RideAssignedBuildReq
      { bookingId :: Id DBooking.Booking,
        newRideInfo :: SyncRide.NewRideInfo
      }
  | RideStartedBuildReq
      { newRideInfo :: SyncRide.NewRideInfo
      }
  | RideCompletedBuildReq
      { newRideInfo :: SyncRide.NewRideInfo,
        rideCompletedInfo :: SyncRide.RideCompletedInfo
      }
  | BookingCancelledBuildReq
      { bookingCancelledInfo :: SyncRide.BookingCancelledInfo,
        mbNewRideInfo :: Maybe SyncRide.NewRideInfo
      }

handler ::
  Id DM.Merchant ->
  DStatusReq ->
  Flow DStatusRes
handler transporterId req = do
  transporter <-
    CQM.findById transporterId
      >>= fromMaybeM (MerchantNotFound transporterId.getId)
  booking <- B.runInReplica $ QRB.findById req.bookingId >>= fromMaybeM (BookingNotFound req.bookingId.getId)
  mbRide <- B.runInReplica $ QRide.findOneByBookingId booking.id
  let transporterId' = booking.providerId
  unless (transporterId' == transporterId) $ throwError AccessDenied
  info <- case mbRide of
    Just ride -> do
      case ride.status of
        DRide.NEW -> do
          newRideInfo <- SyncRide.fetchNewRideInfo ride booking
          pure $ RideAssignedBuildReq {bookingId = booking.id, newRideInfo = newRideInfo}
        DRide.INPROGRESS -> do
          newRideInfo <- SyncRide.fetchNewRideInfo ride booking
          pure $ RideStartedBuildReq {newRideInfo}
        DRide.COMPLETED -> do
          newRideInfo <- SyncRide.fetchNewRideInfo ride booking
          rideCompletedInfo <- SyncRide.fetchRideCompletedInfo ride booking
          pure $ RideCompletedBuildReq {newRideInfo, rideCompletedInfo}
        DRide.CANCELLED -> do
          newRideInfo <- SyncRide.fetchNewRideInfo ride booking
          bookingCancelledInfo <- SyncRide.fetchBookingCancelledInfo (Just ride) booking
          pure $ BookingCancelledBuildReq {mbNewRideInfo = Just newRideInfo, bookingCancelledInfo}
    Nothing -> do
      case booking.status of
        DBooking.NEW -> do
          pure $ NewBookingBuildReq {bookingId = booking.id}
        DBooking.TRIP_ASSIGNED -> do
          throwError (RideNotFound $ "BookingId: " <> booking.id.getId)
        DBooking.COMPLETED -> do
          throwError (RideNotFound $ "BookingId: " <> booking.id.getId)
        DBooking.CANCELLED -> do
          bookingCancelledInfo <- SyncRide.fetchBookingCancelledInfo Nothing booking
          pure $ BookingCancelledBuildReq {mbNewRideInfo = Nothing, bookingCancelledInfo}
  pure DStatusRes {transporter, info}
