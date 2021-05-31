module Beckn.Types.Core.Migration.Descriptor (Descriptor (..)) where

import Beckn.Types.Core.Migration.Image (Image)
import EulerHS.Prelude
import Servant.Client (BaseUrl)

data Descriptor = Descriptor
  { _name :: Maybe Text,
    _code :: Maybe Text,
    _symbol :: Maybe Text,
    _short_desc :: Maybe Text,
    _long_desc :: Maybe Text,
    _images :: Maybe [Image],
    _audio :: Maybe BaseUrl,
    _3d_render :: Maybe BaseUrl
  }
  deriving (Generic, Show)

instance FromJSON Descriptor where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Descriptor where
  toJSON = genericToJSON stripAllLensPrefixOptions
