{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.Error.BaseError.HTTPError.APIError where

import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Error.BaseError.HTTPError.APIError
import Beckn.Utils.Servant.Client
import EulerHS.Prelude
import qualified EulerHS.Types as ET

newtype APICallError = APICallError APIError
  deriving (Show)

instanceExceptionWithParent 'APIException ''APICallError

instance IsBaseError APICallError where
  toMessage (APICallError APIError {..}) =
    Just $
      "Request to own API returned error code " <> errorCode
        <> maybe "" ("with message: " <>) errorMessage

instance IsHTTPError APICallError where
  toErrorCode (APICallError _) = "API_CALL_ERROR"

instance IsAPIError APICallError

callOwnAPI ::
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI env a
callOwnAPI = callApiUnwrappingApiError APICallError
