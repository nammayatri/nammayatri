{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Common where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Data.Generics.Labels ()
import Data.Swagger
import qualified EulerHS.Language as L
import EulerHS.Prelude

type FlowR a = ReaderT a L.Flow

class GuidLike a where
  generateGUID :: FlowR () a

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
    _message :: Ack
  }
  deriving (Show, Generic)

instance FromJSON AckResponse where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON AckResponse where
  toJSON = genericToJSON stripLensPrefixOptions

newtype IdObject = IdObject
  { id :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)
