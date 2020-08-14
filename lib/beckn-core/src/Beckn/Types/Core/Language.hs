module Beckn.Types.Core.Language where

import Data.Text
import EulerHS.Prelude

newtype Language = Language
  { _code :: Text
  }
  deriving (Generic, Show)

instance FromJSON Language where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Language where
  toJSON = genericToJSON stripAllLensPrefixOptions
