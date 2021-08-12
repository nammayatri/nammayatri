module Beckn.Types.APISuccess (APISuccess (..)) where

import Data.Aeson hiding (Success)
import Data.Aeson.Types (parseFail, typeMismatch)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding ((.=))

data APISuccess = Success deriving (Generic, Show, Eq, ToSchema)

instance ToJSON APISuccess where
  toJSON Success = object ["result" .= ("Success" :: Text)]

instance FromJSON APISuccess where
  parseJSON (Object obj) = do
    result :: String <- obj .: "result"
    case result of
      "Success" -> pure Success
      _ -> parseFail "Expected \"Success\" in \"result\" field."
  parseJSON wrongVal = typeMismatch "Object APISuccess" wrongVal
