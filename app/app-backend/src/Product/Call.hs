{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.Call where

import App.Types
import Beckn.External.Encryption
import Beckn.External.Exotel.Flow
import Beckn.External.Exotel.Types
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Utils.Logging
import Data.Semigroup
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import Servant.Client (BaseUrl (..))
import qualified Storage.Queries.CallStatus as QCallStatus
import Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Types.API.Call as CallAPI
import Types.Error
import Types.Storage.CallStatus
import Types.Storage.Person as Person
import qualified Types.Storage.Ride as SRide
import Utils.Common

-- | Try to initiate a call customer -> driver
initiateCallToDriver :: Id SRide.Ride -> Id Person.Person -> FlowHandler CallAPI.CallRes
initiateCallToDriver rideId personId =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    (customerPhone, providerPhone) <- getCustomerAndDriverPhones rideId
    callbackUrl <- buildCallbackUrl
    callId <- generateGUID
    let attachments = ExotelAttachments {callId = getId callId, rideId = getId rideId}
    initiateCall customerPhone providerPhone callbackUrl attachments
    logTagInfo ("RideId: " <> getId rideId) "Call initiated from customer to driver."
    return $ CallAPI.CallRes callId
  where
    buildCallbackUrl = do
      bapUrl <- askConfig (.bapSelfURIs.cabs)
      return $
        bapUrl
          { baseUrlPath = baseUrlPath bapUrl <> "ride/" <> T.unpack (getId rideId) <> "call/statusCallback"
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
        CallStatus
          { id = callId,
            exotelCallSid = req.callSid,
            rideId = rideId,
            status = req.status,
            createdAt = now
          }

getCallStatus :: Id SRide.Ride -> Id CallStatus -> Id Person -> FlowHandler CallAPI.GetCallStatusRes
getCallStatus _ callStatusId _ = withFlowHandlerAPI $ do
  QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> makeCallStatusAPIEntity

getPerson :: (DBFlow m r, EncFlow m r) => SRide.Ride -> m Person
getPerson ride = do
  rideBooking <- QRB.findById ride.bookingId >>= fromMaybeM RideBookingNotFound
  let personId = rideBooking.riderId
  Person.findById personId >>= fromMaybeM PersonNotFound

-- | Get person's mobile phone
getPersonPhone :: EncFlow m r => Person -> m Text
getPersonPhone Person {..} = do
  decMobNum <- decrypt mobileNumber
  let phonenum = (<>) <$> mobileCountryCode <*> decMobNum
  phonenum & fromMaybeM (InternalError "Customer has no phone number.")

-- | Returns phones pair or throws an error
getCustomerAndDriverPhones :: (EncFlow m r, DBFlow m r) => Id SRide.Ride -> m (Text, Text)
getCustomerAndDriverPhones rideId = do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM RideDoesNotExist
  person <- getPerson ride
  customerPhone <- getPersonPhone person
  return (customerPhone, ride.driverMobileNumber)
