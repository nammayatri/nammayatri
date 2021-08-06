module Beckn.Types.Core.Migration.Rating where

import Data.Aeson (withObject, (.!=), (.:?))
import Data.Text (singleton)
import EulerHS.Prelude

data Rating = Rating
  { value :: Maybe Integer, -- FIXME: probably not integer
    unit :: Text,
    max_value :: Integer,
    direction :: RatingDirection
  }
  deriving (Generic, ToJSON, Show, Eq)

instance FromJSON Rating where
  parseJSON = withObject "Rating" $ \o ->
    Rating
      <$> o .:? "value"
      <*> o .:? "unit" .!= singleton (toEnum 0x2B50)
      <*> o .:? "max_value" .!= 5
      <*> o .:? "direction" .!= UP

data RatingDirection = UP | DOWN
  deriving (Generic, Show, Eq, ToJSON, FromJSON)
