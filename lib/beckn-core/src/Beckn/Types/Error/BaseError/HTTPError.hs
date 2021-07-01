module Beckn.Types.Error.BaseError.HTTPError
  ( module Beckn.Types.Error.BaseError.HTTPError,
    module Beckn.Types.Error.BaseError.HTTPError.HttpCode,
    module Beckn.Types.Error.BaseError,
    instanceExceptionWithParent,
  )
where

import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError.HttpCode
import Beckn.Utils.Error.Hierarchy (instanceExceptionWithParent)
import Control.Exception
import EulerHS.Prelude hiding (Show, pack, show)
import Network.HTTP.Types (Header)
import Data.Aeson (Value (Null))

type IsHTTPException e = (IsHTTPError e, Exception e)

class IsBaseError e => IsHTTPError e where
  toErrorCode :: e -> Text

  toHttpCode :: e -> HttpCode
  toHttpCode _ = E500

  toPayload :: e -> Value
  toPayload _ = Null

  toCustomHeaders :: e -> [Header]
  toCustomHeaders _ = []

toMessageIfNotInternal :: IsHTTPError e => e -> Maybe Text
toMessageIfNotInternal e = if isInternalError (toHttpCode e) then Nothing else toMessage e
