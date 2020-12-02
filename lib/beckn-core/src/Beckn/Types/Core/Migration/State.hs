{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.State (State (..)) where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Data.Aeson.TH (deriveJSON)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (State)

data State = State
  { _descriptor :: Maybe Descriptor,
    _updated_at :: Maybe UTCTime,
    _updated_by :: Maybe Text
  }
  deriving (Generic, Show)

deriveJSON stripLensPrefixOptions ''State
