module Domain.Types.Message.MessageTranslation where

import qualified Domain.Types.Message.Message as Msg
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id

data MessageTranslation = MessageTranslation
  { messageId :: Id Msg.Message,
    language :: Language,
    title :: Text,
    label :: Maybe Text,
    description :: Text,
    createdAt :: UTCTime
  }
