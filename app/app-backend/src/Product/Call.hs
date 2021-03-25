{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.Call where

import App.Types
import Beckn.External.Exotel.Flow
import Beckn.Types.Common
import Beckn.Types.Core.API.Call
import Beckn.Types.Core.Ack
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Types.Mobility.Driver as Driver
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common
  ( decodeFromText,
    fromMaybeM500,
    fromMaybeMWithInfo500,
    throwError500,
    throwErrorWithInfo500,
    withFlowHandler,
  )
import Data.Maybe
import Data.Semigroup
import EulerHS.Prelude hiding (id)
import Models.Case as Case
import Models.ProductInstance as ProductInstance
import Storage.Queries.Person as Person
import Types.ProductInfo as ProductInfo

-- | Try to initiate a call customer -> provider
initiateCallToProvider :: Person.Person -> CallReq -> FlowHandler CallRes
initiateCallToProvider _ req = withFlowHandler $ do
  let piId = req ^. #productInstanceId
  (customerPhone, providerPhone) <- getProductAndCustomerPhones $ Id piId
  initiateCall customerPhone providerPhone
  mkAckResponse

-- | Try to initiate a call provider -> customer
initiateCallToCustomer :: CallReq -> FlowHandler CallRes
initiateCallToCustomer req = withFlowHandler $ do
  let piId = req ^. #productInstanceId -- RIDESEARCH PI
  (customerPhone, providerPhone) <- getProductAndCustomerPhones $ Id piId
  initiateCall providerPhone customerPhone
  mkAckResponse

-- | Get customer and driver pair by case Id
getProductAndCustomerInfo :: Id ProductInstance -> Flow (Person, Driver.Driver)
getProductAndCustomerInfo rideSearchPid = do
  prodInst <- ProductInstance.findByParentIdType (Just rideSearchPid) Case.RIDEORDER
  c <- Case.findById $ _caseId prodInst
  personId <- Case._requestor c & fromMaybeM500 CaseRequestorNotFound
  info <- ProductInstance._info prodInst & fromMaybeMWithInfo500 ProductInstanceInvalidState "_info is null. No product info linked to product instance."
  productInfo <- decodeFromText info & fromMaybeMWithInfo500 CommonError "Parse error."
  tracker_ <- _tracker productInfo & fromMaybeMWithInfo500 CommonError "_tracker is null. No tracker info."
  let trip = _trip tracker_
      driver_ = trip ^. #driver
  driver <- driver_ & fromMaybeMWithInfo500 CommonError "_driver is null. No driver info."
  person_ <- Person.findById $ Id personId
  case person_ of
    Just person -> return (person, toBeckn driver)
    Nothing -> throwError500 PersonNotFound

-- | Get person's mobile phone
getPersonPhone :: Person -> Flow Text
getPersonPhone Person {..} = do
  let phonenum = _mobileCountryCode <> _mobileNumber
  phonenum & fromMaybeMWithInfo500 CommonError "Customer has no phone number."

-- | Get phone from Person data type
getPersonTypePhone :: Driver.Driver -> Flow Text
getPersonTypePhone Driver.Driver {..} =
  case phones of
    [] -> throwErrorWithInfo500 CommonError "Driver has no contacts."
    x : _ -> return x

-- | Returns phones pair or throws an error
getProductAndCustomerPhones :: Id ProductInstance -> Flow (Text, Text)
getProductAndCustomerPhones piId = do
  (person, driver) <- getProductAndCustomerInfo piId
  customerPhone <- getPersonPhone person
  driverPhone <- getPersonTypePhone driver
  return (customerPhone, driverPhone)

mkAckResponse :: Flow CallRes
mkAckResponse = return $ Ack {_status = "ACK"}
