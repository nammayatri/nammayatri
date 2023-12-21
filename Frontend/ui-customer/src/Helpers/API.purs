module Helpers.API where

import Prelude
import Engineering.Error.Utils
import JBridge (factoryResetApp, stopChatListenerService)
import Engineering.Helpers.API as API
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, Header(..))
import Foreign.Generic (class Decode)
import Types.App (FlowBT, GlobalState)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalStore, getValueToLocalNativeStore)
import Control.Monad.Trans.Class (lift)
import Presto.Core.Types.Language.Flow (loadS)
import Engineering.Helpers.Commons (liftFlow)
import Data.Array (singleton)
import Data.Maybe (maybe, Maybe(..))

data CustomerDefaultErrorHandler = CustomerDefaultErrorHandler
instance defaultApiErrorHandler :: ApiErrorHandler CustomerDefaultErrorHandler GlobalState where
  handleAllErrors = defaultHandleAllErrors
  handleNetworkError = defaultHandleNetworkError
  handleServerError = defaultHandleServerError
  handleClientError clientErr _ =
    case clientErr of
      AuthenticationError -> do
        deleteValueFromLocalStore REGISTERATION_TOKEN
        deleteValueFromLocalStore REGISTRATION_APPROVED
        lift $ lift $ liftFlow $ stopChatListenerService
        pure $ factoryResetApp ""
      _ -> pure unit
  handleParsingError = defaultHandleParsingError
  handleUnknownError = defaultHandleUnknownError

baseHeaders :: Array Header
baseHeaders = 
  [ Header "Content-Type" "application/json"
  , Header "x-client-version" (getValueToLocalStore VERSION_NAME)
  , Header "x-bundle-version" (getValueToLocalStore BUNDLE_VERSION)
  , Header "session_id" (getValueToLocalStore SESSION_ID)
  , Header "x-device" (getValueToLocalNativeStore DEVICE_DETAILS)
  ]

tokenHeader :: Maybe String -> Array Header
tokenHeader regToken = maybe [] singleton $ (Header "token") <$> regToken

callApiBTWithOptions :: forall a b e.
  ApiErrorHandler e GlobalState =>
  StandardEncode a =>
  Decode b =>
  RestEndpoint a b => 
  a ->
  Array Header ->
  e ->
  FlowBT String b
callApiBTWithOptions payload headers errorHandler = do
  regToken <- lift $ lift $ loadS $ show REGISTERATION_TOKEN
  let headers' = headers <> baseHeaders <> (tokenHeader regToken)
  API.callApiBT payload headers' errorHandler

callApiBT :: forall a b.
  StandardEncode a =>
  Decode b =>
  RestEndpoint a b => 
  a ->
  FlowBT String b
callApiBT payload = 
  callApiBTWithOptions payload [] CustomerDefaultErrorHandler

callGzipApiBTWithOptions :: forall a b e.
  ApiErrorHandler e GlobalState =>
  StandardEncode a =>
  Decode b =>
  RestEndpoint a b =>
  a ->
  Array Header ->
  e ->
  FlowBT String b
callGzipApiBTWithOptions payload headers errorHandler = do
  regToken <- lift $ lift $ loadS $ show REGISTERATION_TOKEN
  let headers' = headers <> baseHeaders <> (tokenHeader regToken)
  API.callGzipApiBT payload headers' errorHandler

callGzipApiBT :: forall a b.
  StandardEncode a =>
  Decode b =>
  RestEndpoint a b =>
  a ->
  FlowBT String b
callGzipApiBT payload =
  callGzipApiBTWithOptions payload [] CustomerDefaultErrorHandler
