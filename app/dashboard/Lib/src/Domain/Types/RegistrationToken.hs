module Domain.Types.RegistrationToken where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Dhall (FromDhall)
import qualified Domain.Types.Person as DPerson

-- Each merchant on BAP side is considered like separate BAP
data ServerName = APP_BACKEND_YATRI | APP_BACKEND_ARDU | BECKN_TRANSPORT | DRIVER_OFFER_BPP
  deriving (Generic, FromDhall, Eq, Show, Read, FromJSON, ToJSON, ToSchema)

data RegistrationToken = RegistrationToken
  { id :: Id RegistrationToken,
    token :: Text,
    personId :: Id DPerson.Person,
    createdAt :: UTCTime,
    serverName :: ServerName
  }
  deriving (Generic, Show)
