module Domain.Types.Webengage where

import Kernel.Prelude
import Kernel.Types.Id

data Webengage = Webengage
  { id :: Id Webengage,
    version :: Text,
    contentTemplateId :: Text,
    principalEntityId :: Text,
    infoMessageId :: Text,
    webMessageId :: Text,
    toNumber :: Text,
    status :: Maybe Text
  }
  deriving (Generic)
