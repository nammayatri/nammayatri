{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.Dashboard.MultipleRideCancel
  ( module Domain.Action.Dashboard.MultipleRideCancel,
    module Reexport,
  )
where

import "dashboard-helper-api" Dashboard.Common as Reexport
import Domain.Types.Booking as DTB
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.CancellationReason
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as SRide
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data BookingCancelledReq = BookingCancelledReq
  { bookingId :: Id DTB.Booking,
    cancellationReasonCode :: CancellationReasonCode,
    cancellationStage :: CancellationStage,
    additionalInfo :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype MultipleRideCancelReq = MultipleRideCancelReq
  { multipleRideCancelInfo :: [BookingCancelledReq]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MultipleRideCancelReq where
  hideSecrets = identity

bookingCancel ::
  EsqDBFlow m r =>
  BookingCancelledReq ->
  m ()
bookingCancel BookingCancelledReq {..} = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bookingId.getId)
  unless (isBookingCancellable booking) $
    throwError (BookingInvalidStatus (show booking.status))
  mbRide <- QRide.findActiveByRBId booking.id
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show SBCR.ByMerchant)
  bookingCancellationReason <- buildBookingCancellationReason booking.id (mbRide <&> (.id))
  DB.runTransaction $ do
    QRB.updateStatus booking.id DTB.CANCELLED
    whenJust mbRide $ \ride -> QRide.updateStatus ride.id SRide.CANCELLED
    QBCR.upsert bookingCancellationReason
    QPFS.updateStatus booking.riderId DPFS.IDLE
  where
    isBookingCancellable booking =
      booking.status `elem` [DTB.NEW, DTB.CONFIRMED, DTB.AWAITING_REASSIGNMENT, DTB.TRIP_ASSIGNED]

buildBookingCancellationReason ::
  (EsqDBFlow m r) =>
  Id DTB.Booking ->
  Maybe (Id SRide.Ride) ->
  m SBCR.BookingCancellationReason
buildBookingCancellationReason bookingId mbRideId = do
  return
    SBCR.BookingCancellationReason
      { bookingId = bookingId,
        rideId = mbRideId,
        source = SBCR.ByMerchant,
        reasonCode = Just $ CancellationReasonCode "BOOKING_NEW_STATUS_MORE_THAN_6HRS",
        reasonStage = Nothing,
        additionalInfo = Nothing
      }

multipleRideCancel ::
  MultipleRideCancelReq ->
  Flow APISuccess
multipleRideCancel req = do
  _ <- mapM bookingCancel req.multipleRideCancelInfo
  pure Success
