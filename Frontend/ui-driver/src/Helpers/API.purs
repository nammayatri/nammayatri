{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.API where

import Prelude
import Engineering.Error.Utils
import JBridge (factoryResetApp, stopChatListenerService, getManufacturerName)
import Engineering.Helpers.API as API
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, Header(..), ErrorResponse)
import Foreign.Generic (class Decode)
import Types.App (FlowBT, GlobalState)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalStore, getValueToLocalNativeStore)
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (loadS, APIResult(..), Flow, fork)
import Engineering.Helpers.Commons (liftFlow)
import Data.Array (singleton)
import Data.Maybe (maybe, Maybe(..))
import Data.Either (Either(..))
import Data.String as DS
import JBridge as JB
import Screens.NoInternetScreen.Handler as NoInternetScreen
import Debug
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.Utils (checkConditionToShowInternetScreen)

data DriverDefaultErrorHandler = DriverDefaultErrorHandler
instance defaultApiErrorHandler :: ApiErrorHandler DriverDefaultErrorHandler GlobalState where
  handleAllErrors = defaultHandleAllErrors
  handleNetworkError = defaultHandleNetworkError
  handleServerError = defaultHandleServerError
  handleClientError clientErr _ =
    case clientErr of
      AuthenticationError -> do
        deleteValueFromLocalStore REGISTERATION_TOKEN
        lift $ lift $ liftFlow $ stopChatListenerService
        pure $ factoryResetApp ""
      _ -> pure unit
  handleParsingError = defaultHandleParsingError
  handleUnknownError = defaultHandleUnknownError

baseHeaders :: Array Header
baseHeaders = 
  [ Header "Content-Type" "application/json"
  , Header "x-client-version" (getValueToLocalStore VERSION_NAME)
  , Header "x-package" (getValueToLocalStore PACKAGE_NAME)
  , Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION)
  , Header "session_id" (getValueToLocalStore SESSION_ID)
  , Header "x-device" getDeviceDetails
  ]

tokenHeader :: Maybe String -> Array Header
tokenHeader regToken = maybe [] singleton $ (Header "token") <$> regToken

getDeviceDetails :: String
getDeviceDetails = do
  let manufacturer = getManufacturerName unit
  (getValueToLocalNativeStore DEVICE_DETAILS) <> if not $ DS.null manufacturer then "/mf:" <> (getManufacturerName unit) else ""

callApiBTWithOptions :: forall a b e.
  ApiErrorHandler e GlobalState =>
  StandardEncode a =>
  Decode b =>
  RestEndpoint a => 
  a ->
  Array Header ->
  e ->
  FlowBT String b
callApiBTWithOptions payload headers errorHandler = do
  regToken <- lift $ lift $ loadS $ show REGISTERATION_TOKEN
  let headers' = headers <> baseHeaders <> (tokenHeader regToken)
  API.callApiBT payload headers' errorHandler $ noInternetScreenHandler "lazy"

callApiWithOptions :: forall a b.
  StandardEncode a =>
  Decode b =>
  RestEndpoint a => 
  a ->
  Array Header ->
  Flow GlobalState (Either ErrorResponse b)
callApiWithOptions payload headers = do
  regToken <- loadS $ show REGISTERATION_TOKEN
  let headers' = headers <> baseHeaders <> (tokenHeader regToken)
  API.callApi payload headers' $ noInternetScreenHandler "lazy"
   

callApi :: forall a b.
  StandardEncode a =>
  Decode b =>
  RestEndpoint a => 
  a ->
  Flow GlobalState (Either ErrorResponse b)
callApi payload =
  callApiWithOptions payload []

callGzipApiWithOptions :: forall a b.
  StandardEncode a =>
  Decode b =>
  RestEndpoint a =>
  a ->
  Array Header ->
  Flow GlobalState (Either ErrorResponse b)
callGzipApiWithOptions payload headers = do
  regToken <- loadS $ show REGISTERATION_TOKEN
  let headers' = headers <> baseHeaders <> (tokenHeader regToken)
  API.callGzipApi payload headers' $ noInternetScreenHandler "lazy"

callGzipApi :: forall a b.
  StandardEncode a =>
  Decode b =>
  RestEndpoint a =>
  a ->
  Flow GlobalState (Either ErrorResponse b)
callGzipApi payload =
  callGzipApiWithOptions payload []

callApiBT :: forall a b.
  StandardEncode a =>
  Decode b =>
  RestEndpoint a => 
  a ->
  FlowBT String b
callApiBT payload = 
  callApiBTWithOptions payload [] DriverDefaultErrorHandler

callGzipApiBTWithOptions :: forall a b e.
  ApiErrorHandler e GlobalState =>
  StandardEncode a =>
  Decode b =>
  RestEndpoint a =>
  a ->
  Array Header ->
  e ->
  FlowBT String b
callGzipApiBTWithOptions payload headers errorHandler = do
  regToken <- lift $ lift $ loadS $ show REGISTERATION_TOKEN
  let headers' = headers <> baseHeaders <> (tokenHeader regToken)
  API.callGzipApiBT payload headers' errorHandler $ noInternetScreenHandler "lazy"

callGzipApiBT :: forall a b.
  StandardEncode a =>
  Decode b =>
  RestEndpoint a =>
  a ->
  FlowBT String b
callGzipApiBT payload =
  callGzipApiBTWithOptions payload [] DriverDefaultErrorHandler



noInternetScreenHandler :: forall a. a -> Flow GlobalState Unit
noInternetScreenHandler lazy = do
  if checkConditionToShowInternetScreen "lazy" then do
    void  $ fork $ do  
        void $ pure $ JB.hideKeyboardOnNavigation true
        void $ NoInternetScreen.noInternetScreen'
  else 
    pure unit