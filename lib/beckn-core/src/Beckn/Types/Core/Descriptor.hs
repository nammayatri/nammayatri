{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Descriptor where

import Beckn.Types.Core.Image
import Beckn.Utils.Common
import EulerHS.Prelude

data Descriptor = Descriptor
  { _name :: Maybe Text,
    _code :: Maybe Text, -- "SEARCHING-FOR-TRIPS", "TRIP-CONFIRMED", "EN-ROUTE-TO-PICKUP", "AT-PICKUP-LOCATION", "TRIP-STARTED", "TRIP-ENDED", "TRIP-ABORTED"
    _symbol :: Maybe Text,
    _short_desc :: Maybe Text,
    _long_desc :: Maybe Text,
    _images :: Maybe [Image],
    _audio :: Maybe Text,
    _3d_render :: Maybe Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON Descriptor where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Descriptor where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Descriptor where
  example =
    Descriptor
      { _name = Just "Some Name",
        _code = Nothing,
        _symbol = Nothing,
        _short_desc = Just "Short description",
        _long_desc = Just "Long description",
        _images = example,
        _audio = Nothing,
        _3d_render = Nothing
      }

emptyDescriptor :: Descriptor
emptyDescriptor =
  Descriptor
    { _name = Nothing,
      _code = Nothing,
      _symbol = Nothing,
      _short_desc = Nothing,
      _long_desc = Nothing,
      _images = Nothing,
      _audio = Nothing,
      _3d_render = Nothing
    }

withName :: Text -> Descriptor
withName name =
  emptyDescriptor {_name = Just name}

withCode :: Text -> Descriptor
withCode code =
  emptyDescriptor {_code = Just code}
