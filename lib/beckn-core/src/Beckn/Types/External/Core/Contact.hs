module Beckn.Types.External.Core.Contact where

import           Data.Text
import           EulerHS.Prelude

data Contact =
  Contact
    { _email :: Text
    , _mobile :: Mobile
    , _landline :: LandLine
    , _ivr :: [Text]
    }
  deriving (Generic, Show)

instance FromJSON Contact where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Contact where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Mobile =
  Mobile
    { _country_code :: Text 
    , _number :: Text
    }
      deriving (Generic, Show)

instance FromJSON Mobile where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Mobile where
  toJSON = genericToJSON stripAllLensPrefixOptions

data LandLine =
  LandLine
    { _country_code :: Text 
    , _std_code :: Text
    , _number :: Text
    , _extension :: Text
    }
      deriving (Generic, Show)

instance FromJSON LandLine where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON LandLine where
  toJSON = genericToJSON stripAllLensPrefixOptions
