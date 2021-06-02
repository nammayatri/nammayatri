module Types.Issue (Issue (..)) where

import Beckn.Utils.JSON
import Data.Text
import EulerHS.Prelude

data Issue = Issue
  { reason :: Text,
    description :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Issue where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny
