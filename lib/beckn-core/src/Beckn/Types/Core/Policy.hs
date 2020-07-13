module Beckn.Types.Core.Policy where

import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Policy = Policy
  { _id :: Text,
    _type :: Text, -- "CONFIRMATION_POLICY", "PAYMENT_POLICY", "CANCELLATION_POLICY", "REFUND_POLICY"
    _parent_policy_id :: Text,
    _heading :: Text,
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
        _type = "PAYMENT_POLICY",
        _parent_policy_id = idExample,
        _heading = "",
        _terms = example
      }

data PolicyTerm = PolicyTerm
  { _id :: Text,
    _name :: Text,
    _description :: Text
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
        _name = "",
        _description = ""
      }
