module Beckn.Types.Core.Description
  ( Description (..),
  )
where

import EulerHS.Prelude
  ( FromJSON (..),
    Generic,
    Maybe,
    Show,
    Text,
    ToJSON (..),
    genericParseJSON,
    genericToJSON,
    stripAllLensPrefixOptions,
  )

data Description = Description
  { _name :: Text,
    _code :: Text,
    _symbol :: Maybe Text,
    _short_desc :: Maybe Text,
    _long_desc :: Maybe Text,
    _images :: [Text],
    _audio :: Maybe Text,
    _3d_render :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Description where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Description where
  toJSON = genericToJSON stripAllLensPrefixOptions
