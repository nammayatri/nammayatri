module Types.Shard where

import Beckn.Utils.Dhall
import EulerHS.Prelude

data Shard = Shard
  { shardId :: Int,
    shortOrgId :: Text
  }
  deriving (Generic, FromDhall)
