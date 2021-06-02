module Beckn.Types.Mobility.FareProduct where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data FareProduct = FareProduct
  { id :: Text,
    descriptor :: Descriptor,
    policy_id :: Text
  }
  deriving (Generic, Show)

instance FromJSON FareProduct where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON FareProduct where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example FareProduct where
  example =
    FareProduct
      { id = idExample,
        descriptor = example,
        policy_id = idExample
      }
