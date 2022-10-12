module Domain.Types.ServerAccess where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.ServerName as DSN

data ServerAccess = ServerAccess
  { id :: Id ServerAccess,
    serverName :: DSN.ServerName,
    personId :: Id DPerson.Person,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
