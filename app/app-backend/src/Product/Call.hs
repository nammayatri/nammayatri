{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.Call where

import App.Types
import Beckn.External.Encryption
import Beckn.External.Exotel.Flow
import Beckn.Types.Common
import Beckn.Types.Core.API.Call
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Types.Mobility.Driver as Driver
import Beckn.Utils.Logging
import Data.Maybe
import Data.Semigroup
import EulerHS.Prelude hiding (id)
import Storage.Queries.Person as Person
import qualified Storage.Queries.SearchRequest as SearchRequest
import Types.Error
import Types.ProductInfo as ProductInfo
import Types.Storage.Person as Person
import Utils.Common
import qualified Storage.Queries.Ride as QRide
import qualified Types.Storage.Ride as SRide

-- | Try to initiate a call customer -> provider
initiateCallToDriver :: Id SRide.Ride -> Id Person.Person -> FlowHandler CallRes
initiateCallToDriver rideId personId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  (customerPhone, providerPhone) <- getProductAndCustomerPhones rideId
  initiateCall customerPhone providerPhone
  logTagInfo ("RideId:" <> getId rideId) "Call initiated from customer to provider."
  return Ack

-- | Try to initiate a call provider -> customer
initiateCallToCustomer :: Id SRide.Ride -> FlowHandler CallRes
initiateCallToCustomer rideId = withFlowHandlerAPI $ do
  (customerPhone, providerPhone) <- getProductAndCustomerPhones rideId
  initiateCall providerPhone customerPhone
  logTagInfo ("RideId:" <> getId rideId) "Call initiated from provider to customer."
  return Ack

getDriver :: (MonadFlow m) => SRide.Ride -> m Driver.Driver
getDriver ride = do
  info <- ride.info & fromMaybeM (RideFieldNotPresent "info")
  productInfo <- decodeFromText info & fromMaybeM (InternalError "Parse error.")
  tracker_ <- tracker productInfo & fromMaybeM (QuoteFieldNotPresent "tracker")
  let trip_ = trip tracker_
      driver_ = trip_.driver
  driver <- driver_ & fromMaybeM (QuoteFieldNotPresent "driver")
  return $ toBeckn driver

getPerson :: (DBFlow m r, EncFlow m r) => SRide.Ride -> m Person
getPerson ride = do
  searchRequest <- SearchRequest.findById ride.requestId >>= fromMaybeM SearchRequestNotFound
  let personId = searchRequest.requestorId
  Person.findById personId >>= fromMaybeM PersonNotFound

-- | Get person's mobile phone
getPersonPhone :: EncFlow m r => Person -> m Text
getPersonPhone Person {..} = do
  decMobNum <- decrypt mobileNumber
  let phonenum = (<>) <$> mobileCountryCode <*> decMobNum
  phonenum & fromMaybeM (InternalError "Customer has no phone number.")

-- | Get phone from Person data type
getDriverPhone :: (MonadFlow m) => Driver.Driver -> m Text
getDriverPhone Driver.Driver {..} =
  phones & listToMaybe & fromMaybeM (InternalError "Driver has no contacts")

-- | Returns phones pair or throws an error
getProductAndCustomerPhones :: (EncFlow m r, DBFlow m r) => Id SRide.Ride -> m (Text, Text)
getProductAndCustomerPhones rideId = do
  ride <-
    QRide.findById rideId 
      >>= fromMaybeM RideDoesNotExist
  person <- getPerson ride
  driver <- getDriver ride
  customerPhone <- getPersonPhone person
  driverPhone <- getDriverPhone driver
  return (customerPhone, driverPhone)
