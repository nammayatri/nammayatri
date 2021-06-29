{-# LANGUAGE TemplateHaskell #-}

module Beckn.Utils.Error.BaseError.APIError.DomainError where

import Beckn.Types.Error.BaseError.APIError
import Beckn.Types.Error.BaseError.APIError.DomainError
import Beckn.Utils.Servant.Client
import EulerHS.Prelude
import qualified EulerHS.Types as ET

newtype APICallError = APICallError DomainError
  deriving (Show)

instanceExceptionWithParent 'DomainException ''APICallError

instance IsBaseError APICallError where
  toMessage (APICallError DomainError {..}) =
    Just $
      "Request to own API returned error code " <> errorCode
        <> maybe "" ("with message: " <>) errorMessage

instance IsAPIError APICallError where
  toErrorCode (APICallError _) = "API_CALL_ERROR"

instance IsDomainError APICallError

callOwnAPI ::
  Maybe ET.ManagerSelector ->
  Maybe Text ->
  CallAPI env a
callOwnAPI = callApiUnwrappingApiError APICallError
