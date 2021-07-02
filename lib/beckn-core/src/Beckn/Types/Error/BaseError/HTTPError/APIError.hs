module Beckn.Types.Error.BaseError.HTTPError.APIError
  ( module Beckn.Types.Error.BaseError.HTTPError.APIError,
    module Beckn.Types.Error.BaseError.HTTPError.HttpCode,
    module Beckn.Types.Error.BaseError,
  )
where

import Beckn.Types.Error.BaseError
import Beckn.Types.Error.BaseError.HTTPError.FromResponse
import Beckn.Types.Error.BaseError.HTTPError.HttpCode
import Control.Exception
import Data.Aeson (Value)
import EulerHS.Prelude hiding (Show, pack, show)
import Prelude (Show (..))

type IsAPIException e = (IsAPIError e, Exception e)

data APIError = APIError
  { errorCode :: Text,
    errorMessage :: Maybe Text,
    errorPayload :: Value
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance FromResponse APIError where
  fromResponse = fromJsonResponse

class IsAPIError e
