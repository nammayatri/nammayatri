module Beckn.Types.Credentials where

import Beckn.Prelude
import Beckn.Types.Base64
import Beckn.Utils.Dhall

type PrivateKey = Base64

type PublicKey = Base64

data Credential = Credential
  { shortOrgId :: Text,
    uniqueKeyId :: Text,
    signPubKey :: PublicKey,
    url :: BaseUrl
  }
  deriving (Generic, FromDhall)
