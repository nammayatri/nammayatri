module Beckn.Types.Core.Api where

import Beckn.Utils.Example
import Data.Text
import Data.Time
import EulerHS.Prelude hiding (exp)

data Api = Api
  { url :: Text,
    exp :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance Example Api where
  example =
    Api
      { url = "https://app.swaggerhub.com/apis/beckn/",
        exp = example
      }
