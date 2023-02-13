module Domain.Types.Message.MessageTranslation where 

import Kernel.External.Types (Language)
import qualified Domain.Types.Message.Message as Msg
import Kernel.Types.Id
import Kernel.Prelude

data MessageTranslation = MessageTranslation 
  { messageId :: Id Msg.Message,
    language :: Language,
    title :: Text,
    description :: Text,
    createdAt :: UTCTime
  }


