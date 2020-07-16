module Beckn.Types.Core.Offer where

import Beckn.Types.App
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Offer = Offer
  { _id :: Text,
    _name :: Text,
    _code :: Text,
    _ref :: OfferRef
  }
  deriving (Generic, Show)

instance FromJSON Offer where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Offer where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Offer where
  example =
    Offer
      { _id = idExample,
        _name = "Offer #7312",
        _code = "O7312",
        _ref = example
      }

data OfferRef = OfferRef
  { _type :: Text, --"category", "service", "item"
    _ids :: [Text]
  }
  deriving (Generic, Show)

instance FromJSON OfferRef where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON OfferRef where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example OfferRef where
  example =
    OfferRef
      { _type = "service",
        _ids = one idExample
      }
