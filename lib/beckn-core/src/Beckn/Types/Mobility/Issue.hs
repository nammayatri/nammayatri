module Beckn.Types.Mobility.Issue (Issue (..)) where

import Data.Text
import EulerHS.Prelude

data Issue = Issue
  { _reason :: Text,
    _description :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Issue where
  parseJSON = genericParseJSON stripLensPrefixOptions
