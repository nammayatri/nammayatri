module Beckn.Types.Core.Policy where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Policy = Policy
  { _id :: Text,
    _descriptor :: Descriptor,
    _parent_policy_id :: Maybe Text,
    _terms :: [PolicyTerm]
  }
  deriving (Generic, Show)

instance FromJSON Policy where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Policy where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Policy where
  example =
    Policy
      { _id = idExample,
        _descriptor = example,
        _parent_policy_id = Just idExample,
        _terms = example
      }

data PolicyTerm = PolicyTerm
  { _id :: Text,
    _parent_term_id :: Maybe Text,
    _descriptor :: Descriptor
  }
  deriving (Generic, Show)

instance FromJSON PolicyTerm where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON PolicyTerm where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example PolicyTerm where
  example =
    PolicyTerm
      { _id = idExample,
        _parent_term_id = Just idExample,
        _descriptor = example
      }
