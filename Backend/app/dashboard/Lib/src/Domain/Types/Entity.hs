module Domain.Types.Entity where

import Kernel.Prelude
import Kernel.Types.Id

data Entity = Entity
  { id :: Id Entity,
    entityName :: Text,
    entityShortId :: ShortId Entity,
    deleted :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
