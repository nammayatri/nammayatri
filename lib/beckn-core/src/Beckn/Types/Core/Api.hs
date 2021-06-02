module Beckn.Types.Core.Api where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import Data.Time
import EulerHS.Prelude

data Api = Api
  { url :: Text,
    exp :: UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Api where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Api where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Api where
  example =
    Api
      { url = "https://app.swaggerhub.com/apis/beckn/",
        exp = example
      }
