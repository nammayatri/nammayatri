module Beckn.Types.API.Registration where

import           Beckn.Types.Common
import           Beckn.Types.Storage.Customer
import           Data.Swagger
import           EulerHS.Prelude
import           Servant.Swagger

data InitiateLoginReq =
  InitiateLoginReq
    { _medium :: Medium
    , __type  :: LoginType
    , __value :: Text
    }
  deriving (Generic, ToSchema)

instance FromJSON InitiateLoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data InitiateLoginRes =
  InitiateLoginRes
    { tokenId :: Text
    , attemps :: Int
    }
  deriving (Generic, ToJSON, ToSchema)

---------- Verify Login --------
data LoginReq =
  LoginReq
    { _medium :: Medium
    , __type  :: LoginType
    , __value :: String
    , _action :: LoginMode
    }
  deriving (Generic, ToSchema)

instance FromJSON LoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data LoginRes =
  LoginRes
    { registrationToken :: Text
    , customer          :: Customer
    }
  deriving (Generic, ToJSON, ToSchema)
