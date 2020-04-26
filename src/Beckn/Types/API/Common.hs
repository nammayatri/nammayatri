module Beckn.Types.API.Common where

import EulerHS.Prelude

data ErrorResponse =
  ErrorResponse
    { status :: Text
    , responseCode :: Text
    , responseMessage :: Text
    }
  deriving (Show, Generic, ToJSON)
