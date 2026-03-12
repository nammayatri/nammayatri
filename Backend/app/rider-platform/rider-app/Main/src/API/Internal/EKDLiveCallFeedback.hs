module API.Internal.EKDLiveCallFeedback
  ( API,
    handler,
  )
where

import Data.Aeson hiding (Success)
import Data.OpenApi (ToSchema)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify

newtype EKDLiveCallFeedbackReq = EKDLiveCallFeedbackReq {rideId :: Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API =
  ( "ekdLiveCallFeedback"
      :> Header "token" Text
      :> ReqBody '[JSON] EKDLiveCallFeedbackReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler = postEkdLiveCallFeedback

postEkdLiveCallFeedback :: Maybe Text -> EKDLiveCallFeedbackReq -> FlowHandler APISuccess
postEkdLiveCallFeedback mbToken req = withFlowHandlerAPI $ do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  logInfo "Received internal notification request for EKD live call feedback"
  ride <- B.runInReplica $ QRide.findByBPPRideId (Id req.rideId) >>= fromMaybeM (RideDoesNotExist req.rideId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  logDebug $ "Sending EKD_LIVE_CALL_FEEDBACK FCM to rider for rideId: " <> req.rideId <> ", bookingId: " <> ride.bookingId.getId <> ", riderId: " <> booking.riderId.getId
  Notify.notifyRiderOnEKDLiveCallFeedback booking
  logDebug $ "Successfully sent EKD_LIVE_CALL_FEEDBACK FCM to rider for rideId: " <> req.rideId
  pure Success
