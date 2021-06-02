module Beckn.Types.Core.Country where

import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data Country = Country
  { standard :: Text, --"ISO 3166-1 ALPHA-2", "ISO 3166-1 ALPHA-3", "ISO 3166-1 NUMERIC)"
    code :: Text
  }
  deriving (Generic, Show)

instance FromJSON Country where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Country where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
