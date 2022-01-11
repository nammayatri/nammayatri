module Beckn.Utils.Registry
  ( lookupKey,
  )
where

import Beckn.Types.Credentials
import Data.Generics.Labels ()
import EulerHS.Prelude

lookupKey :: Text -> [Credential] -> Maybe Credential
lookupKey uniqueKeyId = find (\credential -> credential.uniqueKeyId == uniqueKeyId)
