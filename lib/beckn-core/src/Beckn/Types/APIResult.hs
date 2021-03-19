module Beckn.Types.APIResult (APIResult (..)) where

import Data.Aeson hiding (Success)
import EulerHS.Prelude hiding ((.=))

data APIResult = Success | Failure Text deriving (Show, Eq)

instance ToJSON APIResult where
  toJSON Success = object ["result" .= ("Success" :: Text)]
  toJSON (Failure message) = object ["result" .= ("Failure" :: Text), "message" .= message]

instance FromJSON APIResult where
  parseJSON = withObject "APIResult" $ \obj ->
    Failure <$> obj .: "message" <|> pure Success
