module Beckn.Types.Core.Image where

import Data.Text
import EulerHS.Prelude

data Image = Image
  { _type :: Text, --"url" , "data""
    _url :: Maybe Text,
    _data :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Image where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Image where
  toJSON = genericToJSON stripAllLensPrefixOptions
