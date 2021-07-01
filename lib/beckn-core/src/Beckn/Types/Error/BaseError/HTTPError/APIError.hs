{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Error.BaseError.HTTPError.APIError
  ( module Beckn.Types.Error.BaseError.HTTPError.APIError,
    module Beckn.Types.Error.BaseError.HTTPError.HttpCode,
    module Beckn.Types.Error.BaseError,
    instanceExceptionWithParent,
  )
where

import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Error.BaseError.HTTPError.FromResponse
import Beckn.Types.Error.BaseError.HTTPError.HttpCode
import Control.Exception
import Data.Text (pack)
import EulerHS.Prelude hiding (Show, pack, show)
import Prelude (Show (..))
import Data.Aeson (Value)

type IsAPIException e = (IsAPIError e, Exception e)

data APIError = APIError
  { errorCode :: Text,
    errorMessage :: Maybe Text,
    errorPayload :: Value
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance FromResponse APIError where
  fromResponse = fromJsonResponse

class IsHTTPError e => IsAPIError e

data APIException = forall e. IsAPIException e => APIException e

instance Show APIException where
  show (APIException e) = show e

instance IsBaseError APIException where
  toMessage (APIException e) = toMessage e

instance IsHTTPError APIException where
  toErrorCode (APIException e) = toErrorCode e
  toHttpCode (APIException e) = toHttpCode e
  toCustomHeaders (APIException e) = toCustomHeaders e

instanceExceptionWithParent 'BaseException ''APIException

toAPIError :: IsAPIError e => e -> APIError
toAPIError e =
  APIError
    { errorCode = toErrorCode e,
      errorMessage = toMessageIfNotInternal e,
      errorPayload = toPayload e
    }

toLogMessageAPIError :: IsAPIError e => e -> Text
toLogMessageAPIError err =
  pack (show (toHttpCode err))
    <> " "
    <> toErrorCode err
    <> maybe "" (": " <>) (toMessage err)
