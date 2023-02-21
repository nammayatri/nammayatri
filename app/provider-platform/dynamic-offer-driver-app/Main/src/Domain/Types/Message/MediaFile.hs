module Domain.Types.Message.MediaFile where

import Kernel.Prelude
import Kernel.Types.Id

data MediaType = Video | Audio | Image | AudioLink | VideoLink | ImageLink deriving (Read, Show, Generic, ToSchema, ToJSON, FromJSON)

data MediaFile = MediaFile
  { id :: Id MediaFile,
    _type :: MediaType,
    url :: Text,
    createdAt :: UTCTime
  }
  deriving (Generic)
