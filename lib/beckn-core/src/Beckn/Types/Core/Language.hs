module Beckn.Types.Core.Language where
  
import           Data.Text
import           EulerHS.Prelude

data Language =
  Language
    { _standard :: Text --"ISO 3166-1 ALPHA-2", "ISO 3166-1 ALPHA-3", "ISO 3166-1 NUMERIC)"
    , _code :: Text
    }
      deriving (Generic, Show)

instance FromJSON Language where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Language where
  toJSON = genericToJSON stripAllLensPrefixOptions
