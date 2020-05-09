module Beckn.Types.External.Core.Price where
  
import           Data.Text
import           EulerHS.Prelude

data Price =
  Price
    { _currency :: Text
    , _estimated_value :: Double
    , _computed_value :: Double
    , _listed_value :: Double
    , _offered_value :: Double
    , _unit :: Text
    , _discount :: Double
    , _tax :: Tax
    }
      deriving (Generic, Show)

instance FromJSON Price where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Price where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Tax =
  Tax
    { _computed :: Double
    , _breakup :: [TaxBreakup] 
    }
      deriving (Generic, Show)

instance FromJSON Tax where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Tax where
  toJSON = genericToJSON stripAllLensPrefixOptions

data TaxBreakup =
  TaxBreakup
    { _line_item :: Text
    , _amount :: Double
    }
      deriving (Generic, Show)

instance FromJSON TaxBreakup where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON TaxBreakup where
  toJSON = genericToJSON stripAllLensPrefixOptions
