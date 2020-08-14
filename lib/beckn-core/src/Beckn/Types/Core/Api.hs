module Beckn.Types.Core.Api where

import Beckn.Utils.Common
import Data.Text
import Data.Time
import EulerHS.Prelude

data Api = Api
  { _url :: Text,
    _exp :: UTCTime
  }
  deriving (Generic, Show)

instance FromJSON Api where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Api where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Api where
  example =
    Api
      { _url = "https://app.swaggerhub.com/apis/beckn/",
        _exp = example
      }
