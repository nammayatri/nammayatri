module Core.ConfirmError where

import Beckn.Prelude
import Beckn.Utils.JSON
import Data.Aeson

data ConfirmError = ConfirmError
  { _type :: Text,
    code :: Text,
    message :: Text
  }
  deriving (Generic, Show)

instance FromJSON ConfirmError where
  parseJSON = withObject "Error" $ \o ->
    ConfirmError
      <$> o .: "type"
      <*> o .: "code"
      <*> o .: "message"

instance ToJSON ConfirmError where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
