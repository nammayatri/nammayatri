{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Model where

import Beckn.Types.Core.Image
import Beckn.Utils.Common
import EulerHS.Prelude

data Model = Model
  { _id :: Text,
    -- Core descriptor type
    _name :: Maybe Text,
    _code :: Maybe Text,
    _symbol :: Maybe Text,
    _short_desc :: Maybe Text,
    _long_desc :: Maybe Text,
    _images :: [Image],
    _audio :: Maybe Text,
    _3d_render :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Model where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Model where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Model where
  example =
    Model
      { _id = idExample,
        _name = Just "Some Name",
        _code = Nothing,
        _symbol = Nothing,
        _short_desc = Just "Short description",
        _long_desc = Just "Long description",
        _images = example,
        _audio = Nothing,
        _3d_render = Nothing
      }
