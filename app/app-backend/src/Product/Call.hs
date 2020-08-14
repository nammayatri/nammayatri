{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.Call where

import App.Types
import Beckn.External.Exotel.Flow
import Beckn.Types.API.Call
import Beckn.Types.App
import Beckn.Types.Mobility.Driver as Driver
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common
import Data.Maybe
import Data.Semigroup
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import Models.Case as Case
import Models.ProductInstance as ProductInstance
import Storage.Queries.Person as Person
import Types.Common (toBeckn)
import Types.ProductInfo as ProductInfo

-- | Try to initiate a call customer -> provider
initiateCallToProvider :: Person.Person -> CallReq -> FlowHandler CallRes
initiateCallToProvider _ req = withFlowHandler $ do
  let piId = req ^. #message . #id
  (customerPhone, providerPhone) <- getProductAndCustomerPhones $ ProductInstanceId piId
  initiateCall customerPhone providerPhone
  mkAckResponse piId "initiateCallToProvider"

-- | Try to initiate a call provider -> customer
initiateCallToCustomer :: CallReq -> FlowHandler CallRes
initiateCallToCustomer req = withFlowHandler $ do
  let piId = req ^. #message . #id
  (customerPhone, providerPhone) <- getProductAndCustomerPhones $ ProductInstanceId piId
  initiateCall providerPhone customerPhone
  mkAckResponse piId "initiateCallToCustomer"

-- | Get customer and driver pair by case ID
getProductAndCustomerInfo :: ProductInstanceId -> Flow (Either String (Person, Driver.Driver))
getProductAndCustomerInfo piId = do
  prodInst <- ProductInstance.findById piId
  c <- Case.findById $ _caseId prodInst
  case Case._requestor c of
    Nothing -> pure $ Left "No person linked to case"
    Just personId ->
      case ProductInstance._info prodInst of
        Nothing -> pure $ Left "No product info linked to case"
        Just info ->
          case decodeFromText info of
            Nothing -> pure $ Left "Bad product info"
            Just productInfo ->
              case _tracker productInfo of
                Nothing -> pure $ Left "No tracker info"
                Just tracker_ -> do
                  let trip = _trip tracker_
                  let driver_ = trip ^. #driver
                  case driver_ of
                    Nothing -> pure $ Left "No driver info"
                    Just driver -> do
                      person_ <- Person.findById $ PersonId personId
                      case person_ of
                        Nothing -> pure $ Left "Customer not found"
                        Just person -> pure $ Right (person, toBeckn driver)

-- | Get person's mobile phone
getPersonPhone :: Person -> Flow (Either String Text)
getPersonPhone person =
  pure $ toEither $ countryCode <> number
  where
    errMsg = Left "Customer has no phone number"
    toEither = maybe errMsg Right
    countryCode = _mobileCountryCode person
    number = _mobileNumber person

-- | Get phone from Person data type
getPersonTypePhone :: Driver.Driver -> Flow (Either String Text)
getPersonTypePhone person = pure $
  case phonesLst of
    [] -> Left errMsg
    x : _ -> Right x
  where
    phonesLst = Driver.phones person
    errMsg = "Driver has no contacts"

-- | Returns phones pair or throws an error
getProductAndCustomerPhones :: ProductInstanceId -> Flow (Text, Text)
getProductAndCustomerPhones piId = do
  info <- getProductAndCustomerInfo piId
  case info of
    Left err -> reportError err
    Right (person, driver) -> do
      customerPhone <- getPersonPhone person
      driverPhone <- getPersonTypePhone driver
      case (customerPhone, driverPhone) of
        (Right cp, Right dp) -> pure (cp, dp)
        (Left err, _) -> reportError err
        (_, Left err) -> reportError err
  where
    reportError = throwJsonError404 "call" . T.pack
