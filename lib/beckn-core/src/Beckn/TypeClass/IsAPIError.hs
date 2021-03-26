module Beckn.TypeClass.IsAPIError where

import Data.Aeson.Types
import EulerHS.Prelude
import qualified Servant.Server.Internal as S

data StatusCode
  = E400
  | E401
  | E402
  | E403
  | E404
  | E500
  | E501
  | E503

toServerError :: StatusCode -> S.ServerError
toServerError sc = case sc of
  E400 -> S.err400
  E401 -> S.err401
  E402 -> S.err402
  E403 -> S.err403
  E404 -> S.err404
  E500 -> S.err500
  E501 -> S.err501
  E503 -> S.err503

class IsAPIError domain_error where
  toAPIError :: domain_error -> APIError
  toStatusCode :: domain_error -> StatusCode

data APIError = APIError
  { errorCode :: Text,
    errorMessage :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
