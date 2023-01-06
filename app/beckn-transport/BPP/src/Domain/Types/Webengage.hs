module Domain.Types.Webengage where

import Beckn.Prelude
import Beckn.Types.Id

data Webengage = Webengage
  { id :: Id Webengage,
    version :: Text,
    contentTemplateId :: Text,
    principalEntityId :: Text,
    infoMessageId :: Text,
    webMessageId :: Text,
    toNumber :: Text,
    status :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)