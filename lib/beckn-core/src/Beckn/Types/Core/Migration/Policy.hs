{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Policy where

import Beckn.Types.Core.Migration.Descriptor
import Beckn.Types.Core.Migration.Time
import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

data Policy = Policy
  { _id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _parent_policy_id :: Maybe Text,
    _time :: Maybe Time
  }
  deriving (Generic, Show)

deriveJSON ''Policy 'stripAllLensPrefixOptions
