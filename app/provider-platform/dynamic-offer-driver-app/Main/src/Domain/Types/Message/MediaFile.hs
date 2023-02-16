module Domain.Types.Message.MediaFile where

import Kernel.Prelude
import Kernel.Types.Id

data MediaType = Video | Audio | Image deriving (Read, Show)

data MediaFile = MediaFile
  { id :: Id MediaFile,
    _type :: MediaType,
    url :: BaseUrl,
    createdAt :: UTCTime
  }
  deriving (Generic)
