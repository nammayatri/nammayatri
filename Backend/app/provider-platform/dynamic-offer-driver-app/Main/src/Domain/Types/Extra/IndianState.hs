{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Extra.IndianState where

import Data.Aeson.Types
import Kernel.Prelude
import Kernel.Types.Beckn.Context (IndianState)

-- | A state as a JSON object key, spelt as in its ToJSON/FromJSON instances, e.g. {"TamilNadu": 300}.
instance ToJSONKey IndianState where
  toJSONKey = toJSONKeyText $ \state -> case toJSON state of
    String key -> key
    _ -> show state

instance FromJSONKey IndianState where
  fromJSONKey = FromJSONKeyTextParser (parseJSON . String)
