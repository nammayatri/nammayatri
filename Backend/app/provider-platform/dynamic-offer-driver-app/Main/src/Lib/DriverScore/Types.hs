{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ExistentialQuantification #-}
module Lib.DriverScore.Types where

import Data.Time
import Kernel.Prelude hiding (show)
import GHC.Show (show)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Storage.Esqueleto (EsqDBFlow)
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified SharedLogic.DriverPool.Config as DP

data EventName =
      OnNewSearchRequestForDriver
    -- OnDriverAcceptingSearchRequest
    -- | OnNewRideAssigned
    -- | OnDriverCancellation
    -- | OnDriverLocationUpdate

data Event a = (Show a, EventPayload a) => Event
  { payload :: a
  , createdAt :: UTCTime
  }

instance Show a => Show (Event a) where
  show = show

class EventPayload a where
  handleEvent :: (DP.HasDriverPoolConfig r, Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) => a -> m ()
