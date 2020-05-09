module Beckn.Types.External.Core.Country where
  
import           Data.Text
import           EulerHS.Prelude

data Country =
  Country
    { _standard :: Text --"ISO 3166-1 ALPHA-2", "ISO 3166-1 ALPHA-3", "ISO 3166-1 NUMERIC)"
    , _code :: Text
    }
      deriving (Generic, Show)

instance FromJSON Country where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Country where
  toJSON = genericToJSON stripAllLensPrefixOptions
