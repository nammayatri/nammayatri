module Beckn.Types.Error.APIError
  ( module Beckn.Types.Error.APIError,
    module Beckn.Types.Error.HttpCode,
    instanceExceptionWithParent,
  )
where

import Beckn.Types.Error.HttpCode
import Beckn.Utils.Error.Hierarchy (instanceExceptionWithParent)
import Control.Exception
import Data.Text (pack)
import EulerHS.Prelude hiding (Show, pack, show)
import Network.HTTP.Types (Header)
import Prelude (Show (..))

data APIError = APIError
  { errorCode :: Text,
    errorMessage :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

class IsAPIError e where
  toErrorCode :: e -> Text

  toMessage :: e -> Maybe Text
  toMessage _ = Nothing

  toHttpCode :: e -> HttpCode
  toHttpCode _ = E500

  toCustomHeaders :: e -> [Header]
  toCustomHeaders _ = []

data APIException = forall e. (Exception e, IsAPIError e) => APIException e

instance Show APIException where
  show (APIException e) = show e

instance Exception APIException

toAPIError :: IsAPIError e => e -> APIError
toAPIError e =
  APIError
    { errorCode = toErrorCode e,
      errorMessage = toMessageIfNotInternal e
    }

toMessageIfNotInternal :: IsAPIError e => e -> Maybe Text
toMessageIfNotInternal e = if isInternalError (toHttpCode e) then Nothing else toMessage e

toLogMessageAPIError :: IsAPIError e => e -> Text
toLogMessageAPIError err =
  pack (show (toHttpCode err))
    <> " "
    <> toErrorCode err
    <> maybe "" (": " <>) (toMessage err)
