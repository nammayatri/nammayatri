module Domain.Types.RegistrationToken where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Person as DPerson

data RegistrationToken = RegistrationToken
  { id :: Id RegistrationToken,
    token :: Text,
    personId :: Id DPerson.Person,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
