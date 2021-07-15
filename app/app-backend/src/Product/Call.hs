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
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.SearchRequest as SearchRequest
import Types.Error
import Types.ProductInfo as ProductInfo
import Types.Storage.Person as Person
import Types.Storage.ProductInstance as ProductInstance
import Types.Storage.SearchRequest as SearchRequest
import Utils.Common

-- | Try to initiate a call customer -> provider
initiateCallToDriver :: Id ProductInstance -> Id Person.Person -> FlowHandler CallRes
initiateCallToDriver rideId personId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  (customerPhone, providerPhone) <- getProductAndCustomerPhones rideId
  initiateCall customerPhone providerPhone
  logTagInfo ("ProdInstId:" <> getId rideId) "Call initiated from customer to provider."
  return Ack

-- | Try to initiate a call provider -> customer
initiateCallToCustomer :: Id ProductInstance -> FlowHandler CallRes
initiateCallToCustomer rideId = withFlowHandlerAPI $ do
  (customerPhone, providerPhone) <- getProductAndCustomerPhones rideId
  initiateCall providerPhone customerPhone
  logTagInfo ("ProdInstId:" <> getId rideId) "Call initiated from provider to customer."
  return Ack

getDriver :: (MonadFlow m) => ProductInstance -> m Driver.Driver
getDriver rideSearchPI = do
  info <- ProductInstance.info rideSearchPI & fromMaybeM (PIFieldNotPresent "info")
  productInfo <- decodeFromText info & fromMaybeM (InternalError "Parse error.")
  tracker_ <- tracker productInfo & fromMaybeM (PIFieldNotPresent "tracker")
  let trip_ = trip tracker_
      driver_ = trip_.driver
  driver <- driver_ & fromMaybeM (PIFieldNotPresent "driver")
  return $ toBeckn driver

getPerson :: (DBFlow m r, EncFlow m r) => ProductInstance -> m Person
getPerson rideSearchPI = do
  searchRequest <- SearchRequest.findById rideSearchPI.requestId >>= fromMaybeM SearchRequestNotFound
  personId <- SearchRequest.requestor searchRequest & fromMaybeM (SearchRequestFieldNotPresent "requestor")
  Person.findById (Id personId) >>= fromMaybeM PersonNotFound

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
getProductAndCustomerPhones :: (EncFlow m r, DBFlow m r) => Id ProductInstance -> m (Text, Text)
getProductAndCustomerPhones rideSearchPid = do
  rideSearchPI <-
    ProductInstance.findByParentIdType rideSearchPid ProductInstance.RIDEORDER
      >>= fromMaybeM PIDoesNotExist
  person <- getPerson rideSearchPI
  driver <- getDriver rideSearchPI
  customerPhone <- getPersonPhone person
  driverPhone <- getDriverPhone driver
  return (customerPhone, driverPhone)
