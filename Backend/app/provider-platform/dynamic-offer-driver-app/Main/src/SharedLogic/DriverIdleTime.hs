{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverIdleTime
  ( resetIdleOnRequestSent,
    bankIdleOnOffline,
    resumeIdleOnOnline,
    getIdleTimeSecondsBulk,
  )
where

import qualified Data.Map.Strict as Map
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common

-- Per-driver IDLE-TIME tracking = time since the driver's last received request, EXCLUDING
-- offline gaps. Two Redis keys per driver: the timestamp of the last request received while
-- online (paused/cleared when offline) and the seconds "banked" across previous online spells.
-- idle = banked + (lastRequestAt ? now - lastRequestAt : 0).
mkIdleLastRequestAtKey :: Text -> Text
mkIdleLastRequestAtKey driverId = "driver-offer:Idle:lastReqAt:DriverId-" <> driverId

mkIdleBankedKey :: Text -> Text
mkIdleBankedKey driverId = "driver-offer:Idle:banked:DriverId-" <> driverId

-- 7-day expiry so stale idle-tracking keys self-clean.
idleKeyExpiry :: Redis.ExpirationTime
idleKeyExpiry = 604800

-- On sending a request to a driver: reset the idle clock (lastRequestAt = now, banked = 0).
resetIdleOnRequestSent :: (Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m ()
resetIdleOnRequestSent driverId = Redis.withCrossAppRedis $ do
  now <- getCurrentTime
  Redis.setExp (mkIdleLastRequestAtKey driverId.getId) now idleKeyExpiry
  Redis.setExp (mkIdleBankedKey driverId.getId) (0 :: Double) idleKeyExpiry

-- On going offline: bank the elapsed running idle time and pause (clear lastRequestAt).
bankIdleOnOffline :: (Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m ()
bankIdleOnOffline driverId = Redis.withCrossAppRedis $ do
  now <- getCurrentTime
  mbLast <- Redis.get (mkIdleLastRequestAtKey driverId.getId)
  whenJust mbLast $ \lastAt -> do
    banked <- fromMaybe 0 <$> Redis.get (mkIdleBankedKey driverId.getId)
    Redis.setExp (mkIdleBankedKey driverId.getId) (banked + max 0 (realToFrac (diffUTCTime now lastAt)) :: Double) idleKeyExpiry
    Redis.del (mkIdleLastRequestAtKey driverId.getId)

-- On coming online: resume the idle clock (lastRequestAt = now), keeping the banked amount.
resumeIdleOnOnline :: (Redis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m ()
resumeIdleOnOnline driverId = Redis.withCrossAppRedis $ do
  now <- getCurrentTime
  Redis.setExp (mkIdleLastRequestAtKey driverId.getId) now idleKeyExpiry

-- Fallback cap on drivers per pipelined MGET so a big pool never issues one unboundedly large
-- multi-key Redis read in a single I/O. Overridable per pool via DriverPoolConfig.idleBulkChunkSize.
defaultIdleBulkDriverChunkSize :: Int
defaultIdleBulkDriverChunkSize = 50

chunksOfList :: Int -> [a] -> [[a]]
chunksOfList _ [] = []
chunksOfList n xs
  | n <= 0 = [xs]
  | otherwise = let (a, rest) = splitAt n xs in a : chunksOfList n rest

-- Current idle time in seconds for a whole batch of drivers = banked + running (running is 0 while
-- paused/offline), in pipelined cross-slot MGETs of at most
-- @fromMaybe defaultIdleBulkDriverChunkSize mbChunkSize@ drivers each (banked keys + last-request
-- keys), instead of two Redis reads per driver.
getIdleTimeSecondsBulk :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Int -> [Id DP.Person] -> m (Map.Map (Id DP.Person) Double)
getIdleTimeSecondsBulk mbChunkSize driverIds =
  Map.unions <$> mapM readChunk (chunksOfList (fromMaybe defaultIdleBulkDriverChunkSize mbChunkSize) driverIds)
  where
    readChunk [] = pure Map.empty
    readChunk chunk = do
      now <- getCurrentTime
      let bankedKeys = map (\d -> mkIdleBankedKey d.getId) chunk
          lastKeys = map (\d -> mkIdleLastRequestAtKey d.getId) chunk
      bankedM <- Map.fromList <$> Redis.withCrossAppRedis (Redis.mGetClusterWithKeys @Double bankedKeys)
      lastM <- Map.fromList <$> Redis.withCrossAppRedis (Redis.mGetClusterWithKeys @UTCTime lastKeys)
      let idleFor d =
            let banked = Map.findWithDefault 0 (mkIdleBankedKey d.getId) bankedM
                running = maybe 0 (\lastAt -> max 0 (realToFrac (diffUTCTime now lastAt))) (Map.lookup (mkIdleLastRequestAtKey d.getId) lastM)
             in banked + running
      pure $ Map.fromList [(d, idleFor d) | d <- chunk]
