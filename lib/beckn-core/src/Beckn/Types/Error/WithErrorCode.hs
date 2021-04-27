module Beckn.Types.Error.WithErrorCode where

import Beckn.Types.Error.APIError
import Data.Typeable (cast)
import EulerHS.Prelude

data WithErrorCode a = WithErrorCode Text a
  deriving (Show)

instance IsAPIError a => IsAPIError (WithErrorCode a) where
  toErrorCode (WithErrorCode code _) = code
  toMessage (WithErrorCode _ msg) = toMessage msg
  toHttpCode (WithErrorCode _ msg) = toHttpCode msg
  toCustomHeaders (WithErrorCode _ msg) = toCustomHeaders msg

instance (IsAPIError e, Exception e) => Exception (WithErrorCode e) where
  toException = toException . APIException
  fromException = (\(APIException e) -> cast e) <=< fromException
