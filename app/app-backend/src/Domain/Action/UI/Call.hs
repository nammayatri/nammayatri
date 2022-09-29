module Domain.Action.UI.Call
  ( CallRes (..),
    CallCallbackReq,
    CallCallbackRes,
    MobileNumberResp,
    GetCallStatusRes,
    initiateCallToDriver,
    callStatusCallback,
    directCallStatusCallback,
    getCallStatus,
    getDriverMobileNumber,
  )
where

import Beckn.External.Encryption
import Beckn.External.Exotel.Flow
import Beckn.External.Exotel.Types
import qualified Beckn.External.Exotel.Types as Call
import Beckn.Prelude
import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Utils.Logging
import Data.Text
import qualified Data.Text as T
import Data.Text.Conversions
import Domain.Types.Booking as DRB
import Domain.Types.CallStatus
import qualified Domain.Types.CallStatus as DCS
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Servant.Client (BaseUrl (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Merchant as Merchant
import Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import Tools.Metrics
import Types.Error
import Utils.Common

newtype CallRes = CallRes
  { callId :: Id DCS.CallStatus
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

type CallCallbackReq = Call.ExotelCallCallback

type CallCallbackRes = AckResponse

type MobileNumberResp = Text

type GetCallStatusRes = DCS.CallStatusAPIEntity

-- | Try to initiate a call customer -> driver
initiateCallToDriver ::
  ( EncFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["exotelCfg" ::: Maybe ExotelCfg, "selfUIUrl" ::: BaseUrl]
  ) =>
  Id SRide.Ride ->
  m CallRes
initiateCallToDriver rideId = do
  (customerPhone, providerPhone) <- getCustomerAndDriverPhones rideId
  callbackUrl <- buildCallbackUrl
  callId <- generateGUID
  let attachments = ExotelAttachments {callId = getId callId, rideId = strip (getId rideId)}
  exotelResponse <- initiateCall customerPhone providerPhone callbackUrl attachments
  logTagInfo ("RideId: " <> getId rideId) "Call initiated from customer to driver."
  callStatus <- buildCallStatus rideId callId exotelResponse
  runTransaction $ QCallStatus.create callStatus
  return $ CallRes callId
  where
    buildCallbackUrl = do
      bapUIUrl <- asks (.selfUIUrl)
      let id = T.unpack (strip (getId rideId))
      return $
        bapUIUrl
          { baseUrlPath = baseUrlPath bapUIUrl <> "/ride/" <> id <> "/call/statusCallback"
          }
    buildCallStatus id callId exotelResponse = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callId,
            exotelCallSid = exotelResponse.exoCall.exoSid.getExotelCallSID,
            rideId = id,
            status = exotelResponse.exoCall.exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

callStatusCallback :: EsqDBFlow m r => CallCallbackReq -> m CallCallbackRes
callStatusCallback req = do
  let callId = Id req.customField.callId
  _ <- QCallStatus.findById callId >>= fromMaybeM CallStatusDoesNotExist
  runTransaction $ QCallStatus.updateCallStatus callId req.status req.conversationDuration req.recordingUrl
  return Ack

directCallStatusCallback :: EsqDBFlow m r => Text -> Text -> Text -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ callDuration = do
  let dialCallStatus = fromText dialCallStatus_ :: ExotelCallStatus
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  recordingUrl <- parseBaseUrl recordingUrl_
  runTransaction $ QCallStatus.updateCallStatus callStatus.id dialCallStatus (fromMaybe 0 callDuration) recordingUrl
  return Ack

getDriverMobileNumber :: (EsqDBFlow m r, EncFlow m r) => Text -> Text -> Text -> Text -> m MobileNumberResp
getDriverMobileNumber callSid callFrom_ callTo_ callStatus_ = do
  let callStatus = fromText callStatus_ :: ExotelCallStatus
  let callFrom = dropFirstZero callFrom_
  let callTo = dropFirstZero callTo_
  merchant <- Merchant.findByExoPhone "+91" callTo >>= fromMaybeM (MerchantWithExoPhoneNotFound callTo)
  person <-
    Person.findByRoleAndMobileNumberAndMerchantId USER "+91" callFrom merchant.id
      >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
  bookings <- QRB.findByRiderIdAndStatus person.id [DRB.TRIP_ASSIGNED]
  booking <- fromMaybeM (BookingForRiderNotFound $ getId person.id) (listToMaybe bookings)
  ride <- QRide.findActiveByRBId booking.id >>= fromMaybeM (RideWithBookingIdNotFound $ getId booking.id)
  callId <- generateGUID
  callStatusObj <- buildCallStatus ride.id callId callSid callStatus
  runTransaction $ QCallStatus.create callStatusObj
  return ride.driverMobileNumber
  where
    dropFirstZero = T.dropWhile (== '0')
    buildCallStatus rideId callId exotelCallId exoStatus = do
      now <- getCurrentTime
      return $
        CallStatus
          { id = callId,
            exotelCallSid = exotelCallId,
            rideId = rideId,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

getCallStatus :: EsqDBFlow m r => Id CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> makeCallStatusAPIEntity

getPerson :: (EsqDBFlow m r, EncFlow m r) => SRide.Ride -> m Person
getPerson ride = do
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let personId = booking.riderId
  Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

-- | Get person's mobile phone
getPersonPhone :: EncFlow m r => Person -> m Text
getPersonPhone Person {..} = do
  decMobNum <- mapM decrypt mobileNumber
  let phonenum = (<>) <$> mobileCountryCode <*> decMobNum
  phonenum & fromMaybeM (InternalError "Customer has no phone number.")

-- | Returns phones pair or throws an error
getCustomerAndDriverPhones :: (EncFlow m r, EsqDBFlow m r) => Id SRide.Ride -> m (Text, Text)
getCustomerAndDriverPhones rideId = do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM (RideDoesNotExist rideId.getId)
  person <- getPerson ride
  customerPhone <- getPersonPhone person
  return (customerPhone, ride.driverMobileNumber)
