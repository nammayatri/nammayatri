module Product.Call
  ( CallCallbackRes,
    MobileNumberResp,
    directCallStatusCallback,
    getCustomerMobileNumber,
    getCallStatus,
  )
where

import Beckn.External.Encryption (decrypt)
import Beckn.External.Exotel.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import qualified Data.Text as T
import Data.Text.Conversions
import qualified Domain.Types.CallStatus as SCS
import Environment
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Types.API.Call
import Types.Error
import Utils.Common

directCallStatusCallback :: Text -> Text -> Text -> Maybe Int -> FlowHandler CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ callDuration = do
  let dialCallStatus = fromText dialCallStatus_ :: ExotelCallStatus
  withFlowHandlerAPI $ do
    callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
    recordingUrl <- parseBaseUrl recordingUrl_
    runTransaction $ QCallStatus.updateCallStatus callStatus.id dialCallStatus (fromMaybe 0 callDuration) recordingUrl
    return Ack

getCustomerMobileNumber :: Text -> Text -> Text -> Text -> FlowHandler MobileNumberResp
getCustomerMobileNumber callSid callFrom_ _ callStatus_ = do
  let callStatus = fromText callStatus_ :: ExotelCallStatus
  let callFrom = dropFirstZero callFrom_
  withFlowHandlerAPI $ do
    driver <- QPerson.findByMobileNumber "+91" callFrom >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
    activeRide <- QRide.getActiveByDriverId driver.id >>= fromMaybeM (RideForDriverNotFound $ getId driver.id)
    activeBooking <- QRB.findById activeRide.bookingId >>= fromMaybeM (BookingNotFound $ getId activeRide.bookingId)
    riderId <-
      activeBooking.riderId
        & fromMaybeM (BookingFieldNotPresent "riderId")
    riderDetails <-
      QRD.findById riderId
        >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
    requestorPhone <- decrypt riderDetails.mobileNumber
    callId <- generateGUID
    callStatusObj <- buildCallStatus activeRide.id callId callSid callStatus
    runTransaction $ QCallStatus.create callStatusObj
    return requestorPhone
  where
    dropFirstZero = T.dropWhile (== '0')
    buildCallStatus rideId callId exotelCallId exoStatus = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callId,
            exotelCallSid = exotelCallId,
            rideId = rideId,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

getCallStatus :: Id SCS.CallStatus -> FlowHandler GetCallStatusRes
getCallStatus callStatusId = withFlowHandlerAPI $ do
  QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> SCS.makeCallStatusAPIEntity
