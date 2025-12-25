{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.DriverReferral where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Storage.Queries.DriverReferral as Queries

getNextRefferalCode :: (CacheFlow m r, EsqDBFlow m r) => m Integer
getNextRefferalCode =
  Hedis.safeGet makeLastRefferalCodeKey >>= \case
    Just (_ :: Integer) -> do
      Hedis.incr makeLastRefferalCodeKey
    Nothing -> do
      lastReferralCode <- Queries.getLastRefferalCode
      cacheLastRefferalCode lastReferralCode
      Hedis.incr makeLastRefferalCodeKey

-- this can assign duplicate referralCodes to drivers, but after the first 1000 drivers....then it will start rolling.
-- so ideally this should be used with a filterring criteria of limiting to few drivers somehow (maybe vicinity or something)
getDynamicRefferalCode :: (CacheFlow m r, EsqDBFlow m r) => m Integer
getDynamicRefferalCode = do
  count <- Hedis.incr makeLastDynamicRefferalCodeKey
  pure $ mod count 1000

cacheLastRefferalCode :: (CacheFlow m r) => Integer -> m ()
cacheLastRefferalCode referralCode = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  _ <- Hedis.incrby makeLastRefferalCodeKey referralCode
  Hedis.expire makeLastRefferalCodeKey expTime

makeLastDynamicRefferalCodeKey :: Text
makeLastDynamicRefferalCodeKey = "driver-offer:CachedQueries:DynamicDriverReferral"

makeLastRefferalCodeKey :: Text
makeLastRefferalCodeKey = "driver-offer:CachedQueries:DriverReferral:Id-getNextRefferalCode"

cleaLastRefferalCodeCache :: (CacheFlow m r) => m ()
cleaLastRefferalCodeCache = Hedis.withCrossAppRedis $ do
  Hedis.del makeLastRefferalCodeKey
