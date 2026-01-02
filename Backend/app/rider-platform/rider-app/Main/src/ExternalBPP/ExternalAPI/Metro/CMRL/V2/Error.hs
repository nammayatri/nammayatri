{-# LANGUAGE TemplateHaskell #-}

module ExternalBPP.ExternalAPI.Metro.CMRL.V2.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (fromResponse))
import Network.HTTP.Types (Status (statusCode))
import Servant.Client (ResponseF (responseStatusCode))

data CMRLV2Error
  = CMRLV2BadRequest Text
  | CMRLV2Unauthorized Text
  | CMRLV2InternalError Text
  | CMRLV2ServiceUnavailable Text
  | CMRLV2GateWayTimeOut Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CMRLV2Error

instance FromResponse CMRLV2Error where
  fromResponse resp =
    case statusCode (responseStatusCode resp) of
      400 -> Just $ CMRLV2BadRequest "Bad Request"
      401 -> Just $ CMRLV2Unauthorized "Unauthorized"
      500 -> Just $ CMRLV2InternalError "Internal Server Error"
      503 -> Just $ CMRLV2ServiceUnavailable "Service Unavailable"
      504 -> Just $ CMRLV2GateWayTimeOut "Gateway Timeout"
      _ -> Nothing

instance IsBaseError CMRLV2Error where
  toMessage = \case
    CMRLV2BadRequest msg -> Just msg
    CMRLV2Unauthorized msg -> Just msg
    CMRLV2InternalError msg -> Just msg
    CMRLV2ServiceUnavailable msg -> Just msg
    CMRLV2GateWayTimeOut msg -> Just msg

instance IsHTTPError CMRLV2Error where
  toErrorCode = \case
    CMRLV2BadRequest _ -> "BAD_REQUEST"
    CMRLV2Unauthorized _ -> "UNAUTHORIZED"
    CMRLV2InternalError _ -> "INTERNAL_ERROR"
    CMRLV2ServiceUnavailable _ -> "SERVICE_UNAVAILABLE"
    CMRLV2GateWayTimeOut _ -> "GATEWAY_TIMEOUT"

  toHttpCode = \case
    CMRLV2BadRequest _ -> E400
    CMRLV2Unauthorized _ -> E401
    CMRLV2InternalError _ -> E500
    CMRLV2ServiceUnavailable _ -> E503
    CMRLV2GateWayTimeOut _ -> E504

instance IsAPIError CMRLV2Error
