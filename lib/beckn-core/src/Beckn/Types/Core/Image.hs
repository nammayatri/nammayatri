module Beckn.Types.Core.Image where

import Beckn.Utils.Common
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

instance Example Image where
  example =
    Image
      { _type = "URL",
        _url = Just "https://i.imgur.com/MjeqeUP.gif",
        _data = Nothing
      }
