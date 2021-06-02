module Beckn.Types.Core.Migration.Descriptor (Descriptor (..)) where

import Beckn.Types.Core.Migration.Image (Image)
import Beckn.Utils.JSON
import EulerHS.Prelude
import Servant.Client (BaseUrl)

data Descriptor = Descriptor
  { name :: Maybe Text,
    code :: Maybe Text,
    symbol :: Maybe Text,
    short_desc :: Maybe Text,
    long_desc :: Maybe Text,
    images :: Maybe [Image],
    audio :: Maybe BaseUrl,
    _3d_render :: Maybe BaseUrl
  }
  deriving (Generic, Show)

instance FromJSON Descriptor where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Descriptor where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
