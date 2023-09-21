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

module Storage.CachedQueries.DriverInformation where

import Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.DriverInformation as QDI

totalDrivers :: (CacheFlow m r, MonadFlow m) => Id Merchant -> m (Int, Int)
totalDrivers merchantId =
  Hedis.safeGet makeTotalDriverKey >>= \case
    Just a -> pure a
    Nothing -> cacheByTotalDriverKey /=<< QDI.countDrivers merchantId

makeTotalDriverKey :: Text
makeTotalDriverKey = "driver-offer:CachedQueries:TotalDrivers"

cacheByTotalDriverKey :: (CacheFlow m r) => (Int, Int) -> m ()
cacheByTotalDriverKey totalNumberOfDrivers = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp makeTotalDriverKey totalNumberOfDrivers expTime
