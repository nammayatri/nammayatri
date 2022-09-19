module Domain.Types.ServerAccess where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.RegistrationToken as DReg

data ServerAccess = ServerAccess
  { id :: Id ServerAccess,
    serverName :: DReg.ServerName,
    personId :: Id DPerson.Person,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
