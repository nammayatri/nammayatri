module Types.Shard where

import Beckn.Types.Id
import EulerHS.Prelude
import Types.Storage.Organization

type Shards = Map Int (ShortId Organization)
