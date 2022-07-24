{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.Call where

import App.Types
import Beckn.External.Encryption
import Beckn.External.Exotel.Flow
import Beckn.External.Exotel.Types
import Beckn.Prelude
import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Utils.Logging
import Data.Text
import qualified Data.Text as T
import Data.Text.Conversions
import Domain.Types.CallStatus
import Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import Domain.Types.RideBooking as DRB
import Servant.Client (BaseUrl (..))
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Merchant as Merchant
import Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Types.API.Call as CallAPI
import Types.Error
import Utils.Common

-- | Try to initiate a call customer -> driver
initiateCallToDriver :: Id SRide.Ride -> Id Person.Person -> FlowHandler CallAPI.CallRes
initiateCallToDriver rideId personId =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    (customerPhone, providerPhone) <- getCustomerAndDriverPhones rideId
    callbackUrl <- buildCallbackUrl
    callId <- generateGUID
    let attachments = ExotelAttachments {callId = getId callId, rideId = strip (getId rideId)}
    exotelResponse <- initiateCall customerPhone providerPhone callbackUrl attachments
    logTagInfo ("RideId: " <> getId rideId) "Call initiated from customer to driver."
    callStatus <- buildCallStatus rideId callId exotelResponse
    runTransaction $ QCallStatus.create callStatus
    return $ CallAPI.CallRes callId
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

callStatusCallback :: Id SRide.Ride -> CallAPI.CallCallbackReq -> FlowHandler CallAPI.CallCallbackRes
callStatusCallback _ req = withFlowHandlerAPI $ do
  let callId = Id req.customField.callId
  _ <- QCallStatus.findById callId >>= fromMaybeM CallStatusDoesNotExist
  runTransaction $ QCallStatus.updateCallStatus callId req.status req.conversationDuration req.recordingUrl
  return Ack

directCallStatusCallback :: Text -> Text -> Text -> Maybe Int -> FlowHandler CallAPI.CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ callDuration = withFlowHandlerAPI $ do
  let dialCallStatus = fromText dialCallStatus_ :: ExotelCallStatus
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  recordingUrl <- parseBaseUrl recordingUrl_
  runTransaction $ QCallStatus.updateCallStatus callStatus.id dialCallStatus (fromMaybe 0 callDuration) recordingUrl
  return Ack

getDriverMobileNumber :: Text -> Text -> Text -> Text -> FlowHandler CallAPI.MobileNumberResp
getDriverMobileNumber callSid callFrom_ callTo_ callStatus_ = do
  let callStatus = fromText callStatus_ :: ExotelCallStatus
  let callFrom = dropFirstZero callFrom_
  let callTo = dropFirstZero callTo_
  withFlowHandlerAPI $ do
    merchant <- Merchant.findByExoPhone "+91" callTo >>= fromMaybeM (MerchantWithExoPhoneNotFound callTo)
    person <-
      Person.findByRoleAndMobileNumberAndMerchantId USER "+91" callFrom merchant.id
        >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
    rideBookings <- QRB.findByRiderIdAndStatus person.id DRB.TRIP_ASSIGNED
    rideBooking <- fromMaybeM (RideBookingForRiderNotFound $ getId person.id) (headMaybe rideBookings)
    ride <- QRide.findActiveByRBId rideBooking.id >>= fromMaybeM (RideWithBookingIdNotFound $ getId rideBooking.id)
    callId <- generateGUID
    callStatusObj <- buildCallStatus ride.id callId callSid callStatus
    runTransaction $ QCallStatus.create callStatusObj
    return ride.driverMobileNumber
  where
    dropFirstZero = T.dropWhile (== '0')

    headMaybe :: [a] -> Maybe a
    headMaybe [] = Nothing
    headMaybe (x : _) = Just x

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

getCallStatus :: Id SRide.Ride -> Id CallStatus -> Id Person -> FlowHandler CallAPI.GetCallStatusRes
getCallStatus _ callStatusId _ = withFlowHandlerAPI $ do
  QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> makeCallStatusAPIEntity

getPerson :: (EsqDBFlow m r, EncFlow m r) => SRide.Ride -> m Person
getPerson ride = do
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM (RideBookingNotFound ride.bookingId.getId)
  let personId = rideBooking.riderId
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
