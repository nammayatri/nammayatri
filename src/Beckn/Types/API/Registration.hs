module Beckn.Types.API.Registration where

import Beckn.Types.API.Common
import Beckn.Types.Storage.Customer
import EulerHS.Prelude

data InitiateLoginReq =
  InitiateLoginReq
    { _medium :: Medium
    , __type :: LoginType
    , __value :: Text
    }
  deriving (Generic)

instance FromJSON InitiateLoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data InitiateLoginRes =
  InitiateLoginRes
    { tokenId :: Text
    , attemps :: Int
    }
  deriving (Generic, ToJSON)

---------- Verify Login --------
data LoginReq =
  LoginReq
    { _medium :: Medium
    , __type :: LoginType
    , __value :: String
    , _action :: LoginMode
    }
  deriving (Generic)

instance FromJSON LoginReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data LoginRes =
  LoginRes
    { registrationToken :: Text
    , customer :: Customer
    }
  deriving (Generic, ToJSON)
