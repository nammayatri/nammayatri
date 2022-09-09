{-# LANGUAGE TemplateHaskell #-}

module Tools.Error where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Error.BaseError.HTTPError.FromResponse
import Beckn.Utils.Common hiding (Error)

data Error = Error
  { code :: Text,
    message :: Text,
    details :: Value
  }
  deriving (Show, Generic, ToJSON, FromJSON, IsAPIError)

instance FromResponse Error where
  fromResponse = fromJsonResponse

instance IsHTTPError Error where
  toErrorCode Error {code} = code
  toHttpCode _ = E500

instance IsBaseError Error where
  toMessage Error {message} = Just message

instance IsBecknAPIError Error where
  toType _ = DOMAIN_ERROR

instanceExceptionWithParent 'HTTPException ''Error
