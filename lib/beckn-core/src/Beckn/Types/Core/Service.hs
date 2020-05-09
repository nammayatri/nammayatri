module Beckn.Types.Core.Service where
  
import           Data.Text
import           EulerHS.Prelude
import           Beckn.Types.Core.Catalog
import           Beckn.Types.Core.Provider

data Service =
  Service
    { _id :: Text
    , _catalog :: Catalog
    , _provider :: Provider
    }
      deriving (Generic, Show)

instance FromJSON Service where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Service where
  toJSON = genericToJSON stripAllLensPrefixOptions
