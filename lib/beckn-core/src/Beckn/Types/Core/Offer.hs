module Beckn.Types.Core.Offer where

import Beckn.Types.Core.Descriptor
import Beckn.Utils.Example
import Data.Text
import Data.Time
import EulerHS.Prelude

data Offer = Offer
  { _id :: Text,
    _descriptor :: Descriptor,
    _applies_to :: OfferRef,
    _start_date :: UTCTime,
    _end_date :: UTCTime
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
        _descriptor = example,
        _applies_to = example,
        _start_date = example,
        _end_date = example
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
