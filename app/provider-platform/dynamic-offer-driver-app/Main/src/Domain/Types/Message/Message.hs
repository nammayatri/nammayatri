module Domain.Types.Message.Message where 

import Kernel.Types.Id 
import Data.OpenApi hiding (title, description)
import Kernel.Prelude
import qualified Domain.Types.Message.MediaFile as MF
import Kernel.External.Types (Language)
import Domain.Types.Merchant (Merchant)

data MessageType = Action Text | Read deriving (Generic, ToJSON, FromJSON, ToSchema, Read, Show)

data Message = Message
  { id :: Id Message,
    _type :: MessageType, 
    title :: Text,
    description :: Text,
    mediaFiles :: [Id MF.MediaFile],
    messageTranslations :: [MessageTranslation],
    merchantId :: Id Merchant,
    createdAt :: UTCTime
  }

data MessageTranslation = MessageTranslation
  { language :: Language,
    title :: Text,
    description :: Text,
    createdAt :: UTCTime
  }

data RawMessage = RawMessage
  { id :: Id Message,
    _type :: MessageType, 
    title :: Text,
    description :: Text,
    mediaFiles :: [Id MF.MediaFile],
    merchantId :: Id Merchant,
    createdAt :: UTCTime
  }
