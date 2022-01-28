{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.Call where

import App.Types
import Beckn.External.Encryption
import Beckn.External.Exotel.Flow
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Taxi.API.Call
import Beckn.Types.Id
import Beckn.Utils.Logging
import Data.Semigroup
import EulerHS.Prelude hiding (id)
import Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import Types.Error
import Types.Storage.Person as Person
import qualified Types.Storage.Ride as SRide
import Utils.Common

-- | Try to initiate a call customer -> provider
initiateCallToDriver :: Id SRide.Ride -> Id Person.Person -> FlowHandler CallRes
initiateCallToDriver rideId personId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  (customerPhone, providerPhone) <- getProductAndCustomerPhones rideId
  initiateCall customerPhone providerPhone
  logTagInfo ("RideId:" <> getId rideId) "Call initiated from customer to provider."
  return Ack

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
getProductAndCustomerPhones :: (EncFlow m r, DBFlow m r) => Id SRide.Ride -> m (Text, Text)
getProductAndCustomerPhones rideId = do
  ride <-
    QRide.findById rideId
      >>= fromMaybeM RideDoesNotExist
  person <- getPerson ride
  customerPhone <- getPersonPhone person
  return (customerPhone, ride.driverMobileNumber)
