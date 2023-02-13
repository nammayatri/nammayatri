module Domain.Types.Message.MediaFile where 

import Kernel.Types.Id 
import Kernel.Prelude

data MediaType = Video | Audio | Image deriving (Read, Show)

data MediaFile = MediaFile
  { id :: Id MediaFile,
    _type :: MediaType, 
    url :: BaseUrl,
    createdAt :: UTCTime
  } deriving (Generic)
