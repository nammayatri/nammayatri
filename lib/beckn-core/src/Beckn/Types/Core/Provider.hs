module Beckn.Types.Core.Provider where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Person
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data Provider = Provider
  { id :: Text,
    descriptor :: Descriptor,
    poc :: Maybe Person
  }
  deriving (Generic, Show)

instance FromJSON Provider where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Provider where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Provider where
  example =
    Provider
      { id = idExample,
        descriptor = example,
        poc = example
      }
