module Domain.Types.Message.Message where

import Data.OpenApi hiding (description, title)
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Message.MediaFile as MF
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id

data MessageType = Action Text | Read deriving (Generic, ToJSON, FromJSON, ToSchema, Read, Show)

data Message = Message
  { id :: Id Message,
    _type :: MessageType,
    title :: Text,
    description :: Text,
    label :: Maybe Text,
    mediaFiles :: [Id MF.MediaFile],
    messageTranslations :: [MessageTranslation],
    merchantId :: Id Merchant,
    createdAt :: UTCTime
  }

data MessageTranslation = MessageTranslation
  { language :: Language,
    title :: Text,
    description :: Text,
    label :: Maybe Text,
    createdAt :: UTCTime
  }

data RawMessage = RawMessage
  { id :: Id Message,
    _type :: MessageType,
    title :: Text,
    description :: Text,
    label :: Maybe Text,
    mediaFiles :: [Id MF.MediaFile],
    merchantId :: Id Merchant,
    createdAt :: UTCTime
  }
