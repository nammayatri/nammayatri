module Types.Shard where

import Beckn.Types.Id
import Domain.Types.Organization
import EulerHS.Prelude

type Shards = Map Int (ShortId Organization)
