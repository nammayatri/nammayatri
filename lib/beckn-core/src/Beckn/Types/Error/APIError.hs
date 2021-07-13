module Beckn.Types.Error.APIError
  ( module Beckn.Types.Error.APIError,
    module Beckn.Types.Error.HttpCode,
    instanceExceptionWithParent,
  )
where

import Beckn.Types.Error.FromResponse
import Beckn.Types.Error.HttpCode
import Beckn.Utils.Error.Hierarchy (instanceExceptionWithParent)
import Control.Exception
import Data.Aeson (Value (..))
import Data.Text (pack)
import EulerHS.Prelude hiding (pack)
import Network.HTTP.Types (Header)
import qualified Prelude as P

type IsAPIException e = (IsAPIError e, Exception e)

data APIError = APIError
  { errorCode :: Text,
    errorMessage :: Maybe Text,
    errorPayload :: Value
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance FromResponse APIError where
  fromResponse = fromJsonResponse

class IsAPIError e where
  toErrorCode :: e -> Text

  toMessage :: e -> Maybe Text
  toMessage _ = Nothing

  toHttpCode :: e -> HttpCode
  toHttpCode _ = E500

  toPayload :: e -> Value
  toPayload _ = Null

  toCustomHeaders :: e -> [Header]
  toCustomHeaders _ = []

data APIException = forall e. IsAPIException e => APIException e

instance P.Show APIException where
  show (APIException e) = show e

instance Exception APIException

toAPIError :: IsAPIError e => e -> APIError
toAPIError e =
  APIError
    { errorCode = toErrorCode e,
      errorMessage = toMessageIfNotInternal e,
      errorPayload = toPayload e
    }

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
