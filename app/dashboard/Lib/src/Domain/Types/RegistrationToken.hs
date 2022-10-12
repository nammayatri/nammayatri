module Domain.Types.RegistrationToken (module Domain.Types.RegistrationToken, module Reexport) where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Types.Person.Type as DP
import Domain.Types.ServerName as Reexport

data RegistrationToken = RegistrationToken
  { id :: Id RegistrationToken,
    token :: Text,
    personId :: Id DP.Person,
    createdAt :: UTCTime,
    serverName :: ServerName
  }
  deriving (Generic, Show)
