{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.Call where

import App.Types
import Beckn.External.Exotel.Flow
import Beckn.Types.Common
import Beckn.Types.Core.API.Call
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Types.Mobility.Driver as Driver
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common
  ( decodeFromText,
    fromMaybeM,
    withFlowHandlerAPI,
  )
import Data.Maybe
import Data.Semigroup
import EulerHS.Prelude hiding (id)
import Models.Case as Case
import Models.ProductInstance as ProductInstance
import Storage.Queries.Person as Person
import Types.Error
import Types.ProductInfo as ProductInfo

-- | Try to initiate a call customer -> provider
initiateCallToProvider :: Person.Person -> CallReq -> FlowHandler CallRes
initiateCallToProvider _ req = withFlowHandlerAPI $ do
  let piId = req ^. #productInstanceId
  (customerPhone, providerPhone) <- getProductAndCustomerPhones $ Id piId
  initiateCall customerPhone providerPhone
  mkAckResponse

-- | Try to initiate a call provider -> customer
initiateCallToCustomer :: CallReq -> FlowHandler CallRes
initiateCallToCustomer req = withFlowHandlerAPI $ do
  let piId = req ^. #productInstanceId -- RIDESEARCH PI
  (customerPhone, providerPhone) <- getProductAndCustomerPhones $ Id piId
  initiateCall providerPhone customerPhone
  mkAckResponse

getDriver :: ProductInstance -> Flow Driver.Driver
getDriver rideSearchPI = do
  info <- ProductInstance._info rideSearchPI & fromMaybeM (PIFieldNotPresent "info")
  productInfo <- decodeFromText info & fromMaybeM (InternalError "Parse error.")
  tracker_ <- _tracker productInfo & fromMaybeM (PIFieldNotPresent "tracker")
  let trip = _trip tracker_
      driver_ = trip ^. #driver
  driver <- driver_ & fromMaybeM (PIFieldNotPresent "driver")
  return $ toBeckn driver

getPerson :: ProductInstance -> Flow Person
getPerson rideSearchPI = do
  c <- Case.findById $ _caseId rideSearchPI
  personId <- Case._requestor c & fromMaybeM (CaseFieldNotPresent "requestor")
  Person.findById (Id personId) >>= fromMaybeM PersonNotFound

-- | Get person's mobile phone
getPersonPhone :: Person -> Flow Text
getPersonPhone Person {..} = do
  let phonenum = (<>) <$> _mobileCountryCode <*> _mobileNumber
  phonenum & fromMaybeM (InternalError "Customer has no phone number.")

-- | Get phone from Person data type
getDriverPhone :: Driver.Driver -> Flow Text
getDriverPhone Driver.Driver {..} =
  phones & listToMaybe & fromMaybeM (InternalError "Driver has no contacts")

-- | Returns phones pair or throws an error
getProductAndCustomerPhones :: Id ProductInstance -> Flow (Text, Text)
getProductAndCustomerPhones rideSearchPid = do
  rideSearchPI <- ProductInstance.findByParentIdType rideSearchPid Case.RIDEORDER
  person <- getPerson rideSearchPI
  driver <- getDriver rideSearchPI
  customerPhone <- getPersonPhone person
  driverPhone <- getDriverPhone driver
  return (customerPhone, driverPhone)

mkAckResponse :: Flow CallRes
mkAckResponse = return $ Ack {_status = ACK}
