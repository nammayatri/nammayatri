module Domain.Action.UI.EKDLiveCallFeedback (postEkdLiveCallFeedback) where

import qualified API.Types.UI.EKDLiveCallFeedback as Req
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify

postEkdLiveCallFeedback :: (Req.EKDLiveCallFeedbackReq -> Environment.Flow APISuccess.APISuccess)
postEkdLiveCallFeedback req = do
  logInfo "Received internal notification request for EKD live call feedback"
  ride <- B.runInReplica $ QRide.findByBPPRideId (Id req.rideId) >>= fromMaybeM (RideDoesNotExist req.rideId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)

  logDebug $ "Sending EKD_LIVE_CALL_FEEDBACK FCM to rider for rideId: " <> req.rideId <> ", bookingId: " <> ride.bookingId.getId <> ", riderId: " <> booking.riderId.getId
  Notify.notifyRiderOnEKDLiveCallFeedback booking
  logDebug $ "Successfully sent EKD_LIVE_CALL_FEEDBACK FCM to rider for rideId: " <> req.rideId

  pure APISuccess.Success
