module Beckn.Types.Core.Provider where

import Beckn.Types.Core.Api
import Beckn.Types.Core.Contact
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Provider = Provider
  { _id :: Text,
    _name :: Text,
    _website :: Text,
    _contact :: Contact,
    _api :: Api
  }
  deriving (Generic, Show)

instance FromJSON Provider where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Provider where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Provider where
  example =
    Provider
      { _id = idExample,
        _name = "my provider",
        _website = "http://www.nyan.cat/",
        _contact = example,
        _api = example
      }
