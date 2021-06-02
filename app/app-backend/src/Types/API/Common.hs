module Types.API.Common where

import Beckn.Utils.JSON
import EulerHS.Prelude

data Ack = Ack
  { action :: Text,
    message :: Text
  }
  deriving (Generic, Show)

instance FromJSON Ack where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Ack where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
