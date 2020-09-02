{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Common where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Data.Aeson
import Data.Generics.Labels ()
import Data.Swagger
import qualified EulerHS.Language as L
import EulerHS.Prelude

type FlowR r = ReaderT r L.Flow

class GuidLike a where
  generateGUID :: FlowR r a

instance GuidLike Text where
  generateGUID = L.generateGUID

data ErrorResponse = ErrorResponse
  { status :: Text,
    responseCode :: Text,
    responseMessage :: Text
  }
  deriving (Show, Generic, ToJSON, ToSchema)

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

ack :: Text -> AckMessage
ack = AckMessage . Ack

newtype IdObject = IdObject
  { id :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

class FromBeckn a b where
  fromBeckn :: a -> b

class ToBeckn a b where
  toBeckn :: b -> a

-- This type class is not strictly an isomorphism. We use the name 'Iso' to
-- denote that it is always expected that a type from the beckn spec should
-- have a corresponding type defined by us allowing conversion (which may be
-- lossy) between the two, when defined as an instance of this typeclass.
class (FromBeckn a b, ToBeckn a b) => BecknSpecIso a b
