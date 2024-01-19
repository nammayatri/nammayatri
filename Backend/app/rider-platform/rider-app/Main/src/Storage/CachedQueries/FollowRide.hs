{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.FollowRide where

import Domain.Types.Booking
import Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common

isKeyExists :: (CacheFlow m r, EsqDBFlow m r) => Text -> m Bool
isKeyExists key = do
  Hedis.safeGet key >>= \case
    Just a -> return a
    Nothing -> pure False

makeFollowsRideKey :: Id Booking -> Id Person -> Text
makeFollowsRideKey bookingId personId = "CachedQueries:FollowRide:BookingId:" <> bookingId.getId <> ":PersonId:" <> personId.getId <> ":followsRide"

setPersonFollowsRide :: CacheFlow m r => Id Booking -> Id Person -> m ()
setPersonFollowsRide bookingId personId = do
  let key = makeFollowsRideKey bookingId personId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp key True expTime

clearFollowsRide :: Hedis.HedisFlow m r => Id Booking -> Id Person -> m ()
clearFollowsRide bookingId personId = do
  Hedis.del (makeFollowsRideKey bookingId personId)

decrementFollowRideCount :: CacheFlow m r => Id Person -> m Integer
decrementFollowRideCount personId = do
  let key = makeFollowsRideCounterKey personId
  Hedis.incr key

incrementFollowRideCount :: CacheFlow m r => Id Person -> m Integer
incrementFollowRideCount personId = do
  let key = makeFollowsRideCounterKey personId
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  count <- Hedis.incr key
  when (count == 1) $ Hedis.expire key expTime
  return count

getFollowRideCounter :: (CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe Integer)
getFollowRideCounter person = do
  let key = makeFollowsRideCounterKey person
  Hedis.safeGet key

makeFollowsRideCounterKey :: Id Person -> Text
makeFollowsRideCounterKey personId = "CachedQueries:FollowRide:Counter:PersonId:" <> personId.getId

clearFollowsRideCounter :: Hedis.HedisFlow m r => Id Person -> m ()
clearFollowsRideCounter personId = do
  Hedis.del (makeFollowsRideCounterKey personId)
