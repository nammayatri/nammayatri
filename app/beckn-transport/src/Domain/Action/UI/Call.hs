module Domain.Action.UI.Call
  ( CallRes (..),
    CallCallbackReq,
    CallCallbackRes,
    GetCallStatusRes,
    initiateCallToCustomer,
    callStatusCallback,
    getCallStatus,
  )
where

import Beckn.External.Encryption (decrypt)
import Beckn.External.Exotel.Flow (initiateCall)
import Beckn.External.Exotel.Types
import qualified Beckn.External.Exotel.Types as Call
import Beckn.Prelude
import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Data.Text
import qualified Data.Text as T
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as SRide
import Servant.Client (BaseUrl (..))
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Metrics
import Types.Error
import Utils.Common

newtype CallRes = CallRes
  { callId :: Id SCS.CallStatus
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

type CallCallbackReq = Call.ExotelCallCallback

type CallCallbackRes = AckResponse

type GetCallStatusRes = SCS.CallStatusAPIEntity

initiateCallToCustomer ::
  ( EsqDBFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasFlowEnv m r ["exotelCfg" ::: Maybe ExotelCfg, "selfUIUrl" ::: BaseUrl]
  ) =>
  Id SRide.Ride ->
  m CallRes
initiateCallToCustomer rideId = do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM (RideDoesNotExist rideId.getId)
  rideBooking <-
    QRB.findById ride.bookingId
      >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)

  riderId <-
    rideBooking.riderId
      & fromMaybeM (RideBookingFieldNotPresent "riderId")
  riderDetails <-
    QRD.findById riderId
      >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  requestorPhone <- decrypt riderDetails.mobileNumber
  driverPhone <- getDriverPhone ride
  callbackUrl <- buildCallbackUrl
  callId <- generateGUID
  let attachments = ExotelAttachments {callId = getId callId, rideId = strip (getId rideId)}
  exotelResponse <- initiateCall requestorPhone driverPhone callbackUrl attachments
  logTagInfo ("RideId:" <> getId rideId) "Call initiated from driver to customer."
  let rId = strip (getId rideId)
  callStatus <- buildCallStatus (Id rId) callId exotelResponse
  runTransaction $ QCallStatus.create callStatus
  return $ CallRes callId
  where
    buildCallbackUrl = do
      bppUIUrl <- asks (.selfUIUrl)
      let rideid = T.unpack (strip (getId rideId))
      return $
        bppUIUrl
          { baseUrlPath = baseUrlPath bppUIUrl <> "/driver/ride/" <> rideid <> "/call/statusCallback"
          }

    buildCallStatus rideid callId exoResponse = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callId,
            exotelCallSid = exoResponse.exoCall.exoSid.getExotelCallSID,
            rideId = rideid,
            status = exoResponse.exoCall.exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

callStatusCallback :: (EsqDBFlow m r) => CallCallbackReq -> m CallCallbackRes
callStatusCallback req = do
  let callId = Id req.customField.callId
  _ <- QCallStatus.findById callId >>= fromMaybeM CallStatusDoesNotExist
  runTransaction $ QCallStatus.updateCallStatus callId req.status req.conversationDuration req.recordingUrl
  return Ack

getCallStatus :: (EsqDBFlow m r) => Id SCS.CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> SCS.makeCallStatusAPIEntity

getDriverPhone :: (EsqDBFlow m r, EncFlow m r) => SRide.Ride -> m Text
getDriverPhone ride = do
  let driverId = ride.driverId
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  phonenum <- SP.getPersonNumber driver
  phonenum & fromMaybeM (InternalError "Driver has no phone number.")
