{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Model where

import Beckn.Types.Core.Image
import Beckn.Utils.Example
import Beckn.Utils.JSON
import EulerHS.Prelude

data Model = Model
  { id :: Text,
    -- Core descriptor type
    name :: Maybe Text,
    code :: Maybe Text,
    symbol :: Maybe Text,
    short_desc :: Maybe Text,
    long_desc :: Maybe Text,
    images :: [Image],
    audio :: Maybe Text,
    _3d_render :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Model where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Model where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Model where
  example =
    Model
      { id = idExample,
        name = Just "Some Name",
        code = Nothing,
        symbol = Nothing,
        short_desc = Just "Short description",
        long_desc = Just "Long description",
        images = example,
        audio = Nothing,
        _3d_render = Nothing
      }
