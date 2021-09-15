module Beckn.Types.Core.Image where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import qualified Beckn.Utils.Schema as Schema
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Data.Text
import EulerHS.Prelude

data Image = Image
  { _type :: Text, --"url" , "data""
    label :: Maybe Text,
    url :: Maybe Text,
    _data :: Maybe Text
  }
  deriving (Generic, Show, Eq)

instance ToSchema Image where
  declareNamedSchema = genericDeclareNamedSchema Schema.stripPrefixUnderscoreIfAny

instance FromJSON Image where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Image where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Image where
  example =
    Image
      { _type = "url",
        label = Nothing,
        url = Just "https://i.imgur.com/MjeqeUP.gif",
        _data = Nothing
      }
