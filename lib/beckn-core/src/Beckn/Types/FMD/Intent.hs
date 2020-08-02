module Beckn.Types.FMD.Intent where

import Beckn.Types.Core.Location
import Beckn.Types.Core.Tag
import Beckn.Types.FMD.Package
import Beckn.Utils.Common
import Data.Time.LocalTime
import EulerHS.Prelude

data Intent = Intent
  { _query_string :: Maybe Text,
    _provider_id :: Maybe Text,
    _category_id :: Maybe Text,
    _item_id :: Maybe Text,
    _pickups :: [PickupDrop],
    _drops :: [PickupDrop],
    _packages :: [Package],
    -- FIXME: tags field name clashes with the one from Core.Intent
    -- We have assumed the domain one here takes precedence
    _tags :: Maybe [Tag]
  }
  deriving (Generic, Show)

instance FromJSON Intent where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Intent where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Intent where
  example =
    Intent
      { _query_string = Nothing,
        _provider_id = Nothing,
        _category_id = Nothing,
        _item_id = Nothing,
        _pickups = example,
        _drops = example,
        _packages = example,
        _tags = example
      }

data PickupDrop = PickupDrop
  { _location :: Location,
    _time :: LocalTime
  }
  deriving (Generic, Show)

instance FromJSON PickupDrop where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON PickupDrop where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example PickupDrop where
  example =
    PickupDrop
      { _location = example,
        _time = example
      }
