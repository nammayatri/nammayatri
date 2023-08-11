{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.DriverReferral where

import qualified Data.Text as T
import Domain.Types.DriverReferral as DDR
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.DriverReferral as Queries

getLastRefferalCode :: (CacheFlow m r, EsqDBFlow m r) => m (Id DriverReferral)
getLastRefferalCode =
  Hedis.withCrossAppRedis (Hedis.safeGet makeLastRefferalCodeKey) >>= \case
    Just a -> do
      let referralCodeNumber = read (T.unpack (a.getId)) :: Integer
      let updatedreferralCode = T.pack (show (referralCodeNumber + 1))
      cacheLastRefferalCode (Id updatedreferralCode)
      pure a
    Nothing -> cacheLastRefferalCode /=<< Queries.getLastRefferalCode

cacheLastRefferalCode :: (CacheFlow m r) => Id DriverReferral -> m ()
cacheLastRefferalCode referralCode = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp makeLastRefferalCodeKey referralCode expTime

makeLastRefferalCodeKey :: Text
makeLastRefferalCodeKey = "driver-offer:CachedQueries:DriverReferral:Id-getLastRefferalCode"

cleaLastRefferalCodeCache :: (CacheFlow m r) => m ()
cleaLastRefferalCodeCache = Hedis.withCrossAppRedis $ do
  Hedis.del makeLastRefferalCodeKey
