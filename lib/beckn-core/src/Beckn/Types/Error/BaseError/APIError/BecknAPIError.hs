{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Error.BaseError.APIError.BecknAPIError
  ( module Beckn.Types.Error.BaseError.APIError.BecknAPIError,
    module Beckn.Types.Error.BaseError.APIError,
    Error.Error (..),
    Error.ErrorType (..),
  )
where

import qualified Beckn.Types.Core.Error as Error
import Beckn.Types.Error.BaseError.APIError
import Beckn.Types.Error.BaseError.APIError.FromResponse
import Data.Aeson.Types
import EulerHS.Prelude hiding (Show, show, (.=))
import Prelude (Show (..))

class IsAPIError e => IsBecknAPIError e where
  toType :: e -> Error.ErrorType

  toPath :: e -> Maybe Text
  toPath _ = Nothing

newtype BecknAPIError = BecknAPIError Error.Error
  deriving (Generic, Eq, Show)

instance FromJSON BecknAPIError where
  parseJSON (Object v) = BecknAPIError <$> v .: "error"
  parseJSON invalid =
    prependFailure
      "Parsing BecknAPIError failed, "
      (typeMismatch "Object" invalid)

instance ToJSON BecknAPIError where
  toJSON (BecknAPIError err) = object ["message" .= ack, "error" .= err]
    where
      ack = object ["ack" .= status]
      status = object ["status" .= ("NACK" :: Text)]

instance FromResponse BecknAPIError where
  fromResponse = fromJsonResponse

data BecknAPIException = forall e. (Exception e, IsBecknAPIError e) => BecknAPIException e

instance IsBaseError BecknAPIException where
  toMessage (BecknAPIException e) = toMessage e

instance IsAPIError BecknAPIException where
  toErrorCode (BecknAPIException e) = toErrorCode e
  toHttpCode (BecknAPIException e) = toHttpCode e
  toCustomHeaders (BecknAPIException e) = toCustomHeaders e

instance Show BecknAPIException where
  show (BecknAPIException e) = show e

instanceExceptionWithParent 'APIException ''BecknAPIException

toBecknAPIError :: IsBecknAPIError e => e -> BecknAPIError
toBecknAPIError e =
  BecknAPIError
    Error.Error
      { _type = toType e,
        code = toErrorCode e,
        path = toPath e,
        message = toMessageIfNotInternal e
      }
