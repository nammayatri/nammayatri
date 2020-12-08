module Beckn.Types.Credentials where

import Beckn.Utils.Dhall

data Credential = Credential
  { uniqueKeyId :: Text,
    shortOrgId :: Text,
    signPubKey :: Text
  }
  deriving (Generic, FromDhall)

data SigningKey = SigningKey
  { uniqueKeyId :: Text,
    signPrivKey :: Text
  }
  deriving (Generic, FromDhall)
