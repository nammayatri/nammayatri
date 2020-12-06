module Beckn.Types.Credential where

import EulerHS.Prelude

data Credential = Credential
  { uniqueKeyId :: Text,
    shortOrgId :: Text,
    signPubKey :: Text,
    signPrivKey :: Maybe Text
  }
  deriving (Generic)
