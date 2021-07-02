{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Error.BaseError.HTTPError
  ( module Beckn.Types.Error.BaseError.HTTPError,
    module Beckn.Types.Error.BaseError.HTTPError.HttpCode,
    module Beckn.Types.Error.BaseError.HTTPError.APIError,
    module Beckn.Types.Error.BaseError.HTTPError.BecknAPIError,
    module Beckn.Types.Error.BaseError,
    instanceExceptionWithParent,
  )
where

import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError.APIError
import Beckn.Types.Error.BaseError.HTTPError.BecknAPIError
import Beckn.Types.Error.BaseError.HTTPError.HttpCode
import Beckn.Utils.Error.Hierarchy (instanceExceptionWithParent)
import Control.Exception
import Data.Aeson (Value (Null))
import EulerHS.Prelude hiding (Show, pack, show)
import Network.HTTP.Types (Header)
import Prelude (Show (..))

type IsHTTPException e = (IsHTTPError e, IsAPIError e, IsBecknAPIError e, Exception e)

class IsBaseError e => IsHTTPError e where
  toErrorCode :: e -> Text

  toHttpCode :: e -> HttpCode
  toHttpCode _ = E500

  toPayload :: e -> Value
  toPayload _ = Null

  toCustomHeaders :: e -> [Header]
  toCustomHeaders _ = []

data HTTPException = forall e. (Exception e, IsHTTPException e) => HTTPException e

instance IsBaseError HTTPException where
  toMessage (HTTPException e) = toMessage e

instance IsHTTPError HTTPException where
  toErrorCode (HTTPException e) = toErrorCode e
  toHttpCode (HTTPException e) = toHttpCode e
  toCustomHeaders (HTTPException e) = toCustomHeaders e

instance Show HTTPException where
  show (HTTPException e) = show e

instanceExceptionWithParent 'BaseException ''HTTPException

toMessageIfNotInternal :: IsHTTPError e => e -> Maybe Text
toMessageIfNotInternal e = if isInternalError (toHttpCode e) then Nothing else toMessage e
