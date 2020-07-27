module Types.API.Search where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import EulerHS.Prelude

data AckResponse = AckResponse
  { _context :: Context,
    _message :: Ack,
    _error :: Maybe Error
  }
  deriving (Show, Generic)

instance FromJSON AckResponse where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON AckResponse where
  toJSON = genericToJSON stripLensPrefixOptions

data Ack = Ack
  { _action :: Text,
    _message :: Text
  }
  deriving (Generic, Show)

instance FromJSON Ack where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Ack where
  toJSON = genericToJSON stripAllLensPrefixOptions
