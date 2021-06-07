module Beckn.Types.Core.Policy where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude hiding (id)

data Policy = Policy
  { id :: Text,
    descriptor :: Descriptor,
    parent_policy_id :: Maybe Text,
    terms :: [PolicyTerm]
  }
  deriving (Generic, Show)

instance FromJSON Policy where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Policy where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Policy where
  example =
    Policy
      { id = idExample,
        descriptor = example,
        parent_policy_id = Just idExample,
        terms = example
      }

data PolicyTerm = PolicyTerm
  { id :: Text,
    parent_term_id :: Maybe Text,
    descriptor :: Descriptor
  }
  deriving (Generic, Show)

instance FromJSON PolicyTerm where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON PolicyTerm where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example PolicyTerm where
  example =
    PolicyTerm
      { id = idExample,
        parent_term_id = Just idExample,
        descriptor = example
      }
