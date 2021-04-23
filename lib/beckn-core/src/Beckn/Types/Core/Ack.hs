module Beckn.Types.Core.Ack where

import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Data.Aeson
import EulerHS.Prelude
import qualified Network.HTTP.Types as H

data Status = ACK | NACK deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)

newtype Ack = Ack
  { _status :: Status
  }
  deriving (Generic, Show)

instance FromJSON Ack where
  parseJSON = genericParseJSON stripLensPrefixOptions

instance ToJSON Ack where
  toJSON = genericToJSON stripAllLensPrefixOptions

data AckResponse = AckResponse
  { _context :: Context,
    _message :: AckMessage,
    _error :: Maybe Error
  }
  deriving (Show, Generic)

instance FromJSON AckResponse where
  parseJSON =
    genericParseJSON
      stripAllLensPrefixOptions
        { omitNothingFields = True
        }

instance ToJSON AckResponse where
  toJSON =
    genericToJSON
      stripLensPrefixOptions
        { omitNothingFields = True
        }

newtype AckMessage = AckMessage {_ack :: Ack}
  deriving (Show, Generic)

instance FromJSON AckMessage where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON AckMessage where
  toJSON = genericToJSON stripLensPrefixOptions

ack :: Status -> AckMessage
ack = AckMessage . Ack

data NackResponseError = NackResponseError
  { _context :: Context,
    _error :: Error,
    _status :: H.Status
  }
  deriving (Show)

instance Exception NackResponseError
