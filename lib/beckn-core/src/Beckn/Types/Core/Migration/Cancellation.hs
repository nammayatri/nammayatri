{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Cancellation (Cancellation (..)) where

import Beckn.Types.Common (IdObject)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Option (Option)
import Beckn.Types.Core.Migration.Policy (Policy)
import Beckn.Utils.JSON (constructorsToLowerOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Time (UTCTime)
import EulerHS.Prelude

data Cancellation = Cancellation
  { _type :: Maybe CancellationType,
    _ref_id :: Maybe Text,
    _policies :: [Policy],
    _time :: Maybe UTCTime,
    _cancelled_by :: Maybe Text,
    _reasons :: Maybe Option,
    _selected_reason :: Maybe IdObject,
    _additional_description :: Maybe Descriptor
  }
  deriving (Generic, Show)

data CancellationType = Full | Partial
  deriving (Generic, Show)

deriveJSON stripLensPrefixOptions ''Cancellation
deriveJSON constructorsToLowerOptions ''CancellationType
