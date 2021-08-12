{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Descriptor where

import Beckn.Types.Core.Image
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data Descriptor = Descriptor
  { name :: Maybe Text,
    code :: Maybe Text, -- "SEARCHING-FOR-TRIPS", "TRIP-CONFIRMED", "EN-ROUTE-TO-PICKUP", "AT-PICKUP-LOCATION", "TRIP-STARTED", "TRIP-ENDED", "TRIP-ABORTED"
    symbol :: Maybe Text,
    short_desc :: Maybe Text,
    long_desc :: Maybe Text,
    images :: Maybe [Image],
    audio :: Maybe Text,
    _3d_render :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToSchema)

instance FromJSON Descriptor where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Descriptor where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Descriptor where
  example =
    Descriptor
      { name = Just "Some Name",
        code = Nothing,
        symbol = Nothing,
        short_desc = Just "Short description",
        long_desc = Just "Long description",
        images = example,
        audio = Nothing,
        _3d_render = Nothing
      }

emptyDescriptor :: Descriptor
emptyDescriptor =
  Descriptor
    { name = Nothing,
      code = Nothing,
      symbol = Nothing,
      short_desc = Nothing,
      long_desc = Nothing,
      images = Nothing,
      audio = Nothing,
      _3d_render = Nothing
    }

withName :: Text -> Descriptor
withName name =
  emptyDescriptor {name = Just name}

withCode :: Text -> Descriptor
withCode code =
  emptyDescriptor {code = Just code}
