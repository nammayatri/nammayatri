{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Error.BaseError.APIError.DomainError
  ( module Beckn.Types.Error.BaseError.APIError.DomainError,
    module Beckn.Types.Error.BaseError.APIError.HttpCode,
    module Beckn.Types.Error.BaseError,
    instanceExceptionWithParent,
  )
where

import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.APIError
import Beckn.Types.Error.BaseError.APIError.FromResponse
import Beckn.Types.Error.BaseError.APIError.HttpCode
import Control.Exception
import Data.Text (pack)
import EulerHS.Prelude hiding (Show, pack, show)
import Prelude (Show (..))
import Data.Aeson (Value)

type IsDomainException e = (IsDomainError e, Exception e)

data DomainError = DomainError
  { errorCode :: Text,
    errorMessage :: Maybe Text,
    errorPayload :: Value
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance FromResponse DomainError where
  fromResponse = fromJsonResponse

class IsAPIError e => IsDomainError e

data DomainException = forall e. IsDomainException e => DomainException e

instance Show DomainException where
  show (DomainException e) = show e

instance IsBaseError DomainException where
  toMessage (DomainException e) = toMessage e

instance IsAPIError DomainException where
  toErrorCode (DomainException e) = toErrorCode e
  toHttpCode (DomainException e) = toHttpCode e
  toCustomHeaders (DomainException e) = toCustomHeaders e

instanceExceptionWithParent 'APIException ''DomainException

toDomainError :: IsDomainError e => e -> DomainError
toDomainError e =
  DomainError
    { errorCode = toErrorCode e,
      errorMessage = toMessageIfNotInternal e,
      errorPayload = toPayload e
    }

toLogMessageDomainError :: IsDomainError e => e -> Text
toLogMessageDomainError err =
  pack (show (toHttpCode err))
    <> " "
    <> toErrorCode err
    <> maybe "" (": " <>) (toMessage err)
