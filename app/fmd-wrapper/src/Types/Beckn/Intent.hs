module Types.Beckn.Intent where

import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Time (UTCTime)
import EulerHS.Prelude
import Types.Beckn.Location
import Types.Beckn.Package
import Types.Beckn.Tag

data Intent = Intent
  { query_string :: Maybe Text,
    provider_id :: Maybe Text,
    category_id :: Maybe Text,
    item_id :: Maybe Text,
    pickups :: [PickupDrop],
    drops :: [PickupDrop],
    packages :: Maybe [Package],
    -- FIXME: tags field name clashes with the one from Core.Intent
    -- We have assumed the domain one here takes precedence
    tags :: Maybe [Tag]
  }
  deriving (Generic, Show)

instance FromJSON Intent where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Intent where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example Intent where
  example =
    Intent
      { query_string = Nothing,
        provider_id = Nothing,
        category_id = Nothing,
        item_id = Nothing,
        pickups = example,
        drops = example,
        packages = example,
        tags = example
      }

data PickupDrop = PickupDrop
  { location :: Location,
    time :: Maybe UTCTime
  }
  deriving (Generic, Show)

instance FromJSON PickupDrop where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON PickupDrop where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance Example PickupDrop where
  example =
    PickupDrop
      { location = example,
        time = example
      }
