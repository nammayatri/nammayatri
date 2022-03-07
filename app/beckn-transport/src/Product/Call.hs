module Product.Call where

import App.Types
import Beckn.External.Encryption (decrypt)
import Beckn.External.Exotel.Flow (initiateCall)
import Beckn.External.Exotel.Types
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Data.Text
import qualified Data.Text as T
import EulerHS.Prelude
import Servant.Client (BaseUrl (..))
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RiderDetails as QRD
import qualified Types.API.Call as CallAPI
import Types.Error
import qualified Types.Storage.CallStatus as SCS
import qualified Types.Storage.Person as SP
import qualified Types.Storage.Ride as SRide
import Utils.Common

initiateCallToCustomer :: Id SRide.Ride -> Id SP.Person -> FlowHandler CallAPI.CallRes
initiateCallToCustomer rideId _ = withFlowHandlerAPI $ do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM RideDoesNotExist
  rideBooking <-
    QRB.findById ride.bookingId
      >>= fromMaybeM RideBookingNotFound
  riderDetails <-
    QRD.findById rideBooking.riderId
      >>= fromMaybeM RiderDetailsNotFound
  requestorPhone <- decrypt riderDetails.mobileNumber
  driverPhone <- getDriverPhone ride
  callbackUrl <- buildCallbackUrl
  callId <- generateGUID
  let attachments = ExotelAttachments {callId = getId callId, rideId = strip (getId rideId)}
  initiateCall requestorPhone driverPhone callbackUrl attachments
  logTagInfo ("RideId:" <> getId rideId) "Call initiated from driver to customer."
  return $ CallAPI.CallRes callId
  where
    buildCallbackUrl = do
      bapUrl <- askConfig (.exotelCallbackUrl)
      let rideid = T.unpack (strip (getId rideId))
      return $
        bapUrl
          { baseUrlPath = baseUrlPath bapUrl <> "/driver/ride/" <> rideid <> "/call/statusCallback"
          }

callStatusCallback :: Id SRide.Ride -> CallAPI.CallCallbackReq -> FlowHandler CallAPI.CallCallbackRes
callStatusCallback _ req = withFlowHandlerAPI $ do
  callStatus <- buildCallStatus
  QCallStatus.create callStatus
  return Ack
  where
    buildCallStatus = do
      now <- getCurrentTime
      let callId = Id req.customField.callId
          rideId = Id req.customField.rideId
      return $
        SCS.CallStatus
          { id = callId,
            exotelCallSid = req.callSid,
            rideId = rideId,
            status = req.status,
            recordingUrl = req.recordingUrl,
            conversationDuration = req.conversationDuration,
            createdAt = now
          }

getCallStatus :: Id SRide.Ride -> Id SCS.CallStatus -> Id SP.Person -> FlowHandler CallAPI.GetCallStatusRes
getCallStatus _ callStatusId _ = withFlowHandlerAPI $ do
  QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> SCS.makeCallStatusAPIEntity

getDriverPhone :: (DBFlow m r, EncFlow m r) => SRide.Ride -> m Text
getDriverPhone ride = do
  let driverId = ride.driverId
  driver <- QPerson.findPersonById driverId >>= fromMaybeM PersonNotFound
  decMobNum <- decrypt driver.mobileNumber
  let phonenum = (<>) <$> driver.mobileCountryCode <*> decMobNum
  phonenum & fromMaybeM (InternalError "Driver has no phone number.")
