module Beckn.Types.Core.Offer where
  
import           Data.Text
import           EulerHS.Prelude

data Offer =
  Offer
    { _id :: Text
    , _name :: Text
    , _code :: Text
    , _ref :: OfferRef
    }
      deriving (Generic, Show)

instance FromJSON Offer where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Offer where
  toJSON = genericToJSON stripAllLensPrefixOptions

data OfferRef =
  OfferRef
    { _type :: Text --"category", "service", "item"
    , _ids :: [Text]
    }
      deriving (Generic, Show)

instance FromJSON OfferRef where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON OfferRef where
  toJSON = genericToJSON stripAllLensPrefixOptions
