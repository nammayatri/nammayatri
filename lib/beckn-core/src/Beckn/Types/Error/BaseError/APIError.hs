{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Error.BaseError.APIError
  ( module Beckn.Types.Error.BaseError.APIError,
    module Beckn.Types.Error.BaseError.APIError.HttpCode,
    module Beckn.Types.Error.BaseError,
    instanceExceptionWithParent,
  )
where

import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.APIError.HttpCode
import Beckn.Utils.Error.Hierarchy (instanceExceptionWithParent)
import Control.Exception
import Data.Aeson (Value (..))
import Data.Text (pack)
import EulerHS.Prelude hiding (pack)
import Network.HTTP.Types (Header)
import qualified Prelude as P

type IsAPIException e = (IsAPIError e, Exception e)

class IsBaseError e => IsAPIError e where
  toErrorCode :: e -> Text

  toHttpCode :: e -> HttpCode
  toHttpCode _ = E500

  toPayload :: e -> Value
  toPayload _ = Null

  toCustomHeaders :: e -> [Header]
  toCustomHeaders _ = []

data APIException = forall e. IsAPIException e => APIException e

instance P.Show APIException where
  show (APIException e) = show e

instance IsBaseError APIException where
  toMessage (APIException e) = toMessage e

instanceExceptionWithParent 'BaseException ''APIException

toMessageIfNotInternal :: IsAPIError e => e -> Maybe Text
toMessageIfNotInternal e = if isInternalError (toHttpCode e) then Nothing else toMessage e

toLogMessageAPIError :: IsAPIError e => e -> Text
toLogMessageAPIError err =
  pack (show (toHttpCode err))
    <> " "
    <> toErrorCode err
    <> maybe "" (": " <>) (toMessage err)
    <> showPayload (toPayload err)
  where
    showPayload Null = ""
    showPayload p = "; Payload: " <> show p
