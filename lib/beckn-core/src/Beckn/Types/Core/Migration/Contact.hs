{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Contact (Contact (..)) where

import Beckn.Types.Core.Migration.Tags (Tags)
import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

-- allOf union
data Contact = Contact
  { _phone :: Maybe Text,
    _email :: Maybe Text,
    _tags :: Maybe Tags
  }
  deriving (Generic, Show)

deriveJSON ''Contact 'stripLensPrefixOptions
