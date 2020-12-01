{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Common where

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
