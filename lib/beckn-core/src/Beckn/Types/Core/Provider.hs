module Beckn.Types.Core.Provider where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Person
import Beckn.Utils.Example
import Data.Text
import EulerHS.Prelude

data Provider = Provider
  { _id :: Text,
    _descriptor :: Descriptor,
    _poc :: Maybe Person
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
        _descriptor = example,
        _poc = example
      }
