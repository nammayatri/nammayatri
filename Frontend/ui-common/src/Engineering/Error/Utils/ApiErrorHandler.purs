module Engineering.Error.Utils.ApiErrorHandler where

import Prelude
import Engineering.Error.Types
import Common.Types.App (FlowBT)
import JBridge (toggleBtnLoader)

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
defaultHandleNetworkError _ _ = pure unit

defaultHandleServerError :: forall e st. String -> e -> FlowBT String st Unit
defaultHandleServerError _ _ = pure unit

defaultHandleClientError :: forall e st. ClientErrorType -> e -> FlowBT String st Unit 
defaultHandleClientError _ _ = pure unit

defaultHandleParsingError :: forall e st. String -> e -> FlowBT String st Unit
defaultHandleParsingError _ _ = pure unit

defaultHandleUnknownError :: forall e st. String -> e -> FlowBT String st Unit
defaultHandleUnknownError _ _ = pure unit