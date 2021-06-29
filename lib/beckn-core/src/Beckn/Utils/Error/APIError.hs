{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.Error.APIError where

import Beckn.Types.Error.BaseError.APIError
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

instance IsAPIError APICallError where
  toErrorCode (APICallError _) = "API_CALL_ERROR"

callOwnAPI ::
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI env a
callOwnAPI = callApiUnwrappingApiError APICallError
