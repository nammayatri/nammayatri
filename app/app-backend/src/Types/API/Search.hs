module Types.API.Search where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import EulerHS.Prelude
import Types.API.Common

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
