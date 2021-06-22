module Types.Shard where

import Beckn.Types.Id
import Beckn.Types.Storage.Organization
import EulerHS.Prelude

type Shards = Map Int (ShortId Organization)
