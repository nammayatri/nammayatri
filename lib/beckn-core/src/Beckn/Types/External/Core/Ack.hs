module Beckn.Types.External.Core.Ack where
  
import           Data.Text
import           EulerHS.Prelude

data Ack =
  Ack
    { _action  :: Text
    , _message :: Text
    } deriving (Generic, Show)

instance FromJSON Ack where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Ack where
  toJSON = genericToJSON stripAllLensPrefixOptions
