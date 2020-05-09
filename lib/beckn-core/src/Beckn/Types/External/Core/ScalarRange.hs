module Beckn.Types.External.Core.ScalarRange where
  
import           Data.Text
import           EulerHS.Prelude
import           Beckn.Types.External.Core.Contact
import           Beckn.Types.External.Core.Api

data ScalarRange =
  ScalarRange
    { _min :: Double
    , _max :: Double
    , _unit :: Text
    }
      deriving (Generic, Show)

instance FromJSON ScalarRange where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ScalarRange where
  toJSON = genericToJSON stripAllLensPrefixOptions
