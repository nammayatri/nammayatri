module Engineering.Error.Utils.ApiErrorHandler where

import Engineering.Error.Types
import Engineering.Error.Utils.ApiResponseConverter
import Prelude

import Common.Types.App (FlowBT)
import JBridge (toggleBtnLoader)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))

class ApiErrorHandler e st where
  handleAllErrors :: e -> FlowBT String st Unit
  handleNetworkError :: NetworkErrorType -> e -> FlowBT String st Unit
  handleServerError :: String -> e -> FlowBT String st Unit
  handleClientError :: ClientErrorType -> e -> FlowBT String st Unit
  handleParsingError :: String -> e -> FlowBT String st Unit
  handleUnknownError :: String -> e -> FlowBT String st Unit

defaultHandleAllErrors :: forall e st. e -> FlowBT String st Unit
defaultHandleAllErrors _ = pure $ toggleBtnLoader "" false

defaultHandleNetworkError :: forall e st. NetworkErrorType -> e -> FlowBT String st Unit
defaultHandleNetworkError e _ =  case e of 
  ConnectionRefused -> void $ pure $  JB.toast $ getString NETWORK_ERROR <> " : " <> getString CONNECTION_REFUSED
  Timeout -> void $ pure $ JB.toast $ getString NETWORK_ERROR <> " : " <> getString TIMEOUT
  OtherNetworkError errorMessage -> void $ pure $ JB.toast $ getString NETWORK_ERROR <> " : " <> errorMessage

defaultHandleServerError :: forall e st. String -> e -> FlowBT String st Unit
defaultHandleServerError errorMessage _ = void $ pure $ JB.toast (getString SERVER_ERROR <> " : " <> errorMessage)

defaultHandleClientError :: forall e st. ClientErrorType -> e -> FlowBT String st Unit 
defaultHandleClientError _ _ = pure unit

defaultHandleParsingError :: forall e st. String -> e -> FlowBT String st Unit
defaultHandleParsingError _ _ = pure unit

defaultHandleUnknownError :: forall e st. String -> e -> FlowBT String st Unit
defaultHandleUnknownError _ _ = pure unit 