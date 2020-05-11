module Epass.Types.API.Registration where

import Data.Swagger
import Epass.Types.Common
import Epass.Types.Storage.Customer
import Beckn.Types.Storage.RegistrationToken
import Epass.Types.Storage.User
import EulerHS.Prelude
import Servant.Swagger

data InitiateLoginReq = InitiateLoginReq
  { _medium :: Medium,
    __type :: LoginType,
    _identifier :: Text,
    _role :: Maybe CustomerRole,
    _entityType :: RTEntityType
  }
  deriving (Generic, ToSchema)

instance FromJSON InitiateLoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data ReInitiateLoginReq = ReInitiateLoginReq
  { _medium :: Medium,
    __type :: LoginType,
    _identifier :: Text,
    _entityType :: RTEntityType
  }
  deriving (Generic, ToSchema)

instance FromJSON ReInitiateLoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data InitiateLoginRes = InitiateLoginRes
  { tokenId :: Text,
    attempts :: Int
  }
  deriving (Generic, ToJSON, ToSchema)

---------- Verify Login --------
data LoginReq = LoginReq
  { _medium :: Medium,
    __type :: LoginType,
    _hash :: Text,
    _identifier :: Text
  }
  deriving (Generic, ToSchema)

instance FromJSON LoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data LoginRes = LoginRes
  { registrationToken :: Text,
    customer :: Maybe Customer,
    user :: Maybe User
  }
  deriving (Generic, ToJSON, ToSchema)
