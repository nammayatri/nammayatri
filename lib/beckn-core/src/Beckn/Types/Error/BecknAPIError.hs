{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Error.BecknAPIError
  ( module Beckn.Types.Error.BecknAPIError,
    module Beckn.Types.Error.APIError,
    Error.Error (..),
    Error.ErrorType (..),
  )
where

import qualified Beckn.Types.Core.Error as Error
import Beckn.Types.Error.APIError
import Data.Aeson.Types
import EulerHS.Prelude hiding (Show, show, (.=))
import Prelude (Show (..))

class IsAPIError e => IsBecknAPIError e where
  toType :: e -> Error.ErrorType

  -- TODO: merge with app/fmd-wrapper/src/Types/Error.hs ErrorType
  toBecknCode :: e -> Text
  toBecknCode = toErrorCode

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

data BecknAPIException = forall e. (Exception e, IsBecknAPIError e) => BecknAPIException e

instance IsAPIError BecknAPIException where
  toErrorCode (BecknAPIException e) = toErrorCode e
  toMessage (BecknAPIException e) = toMessage e
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
        _code = toBecknCode e,
        _path = toPath e,
        _message = toMessageIfNotInternal e
      }
