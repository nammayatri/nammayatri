{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Maps.DirectionsCache where

import Data.Text
import Domain.Types.Maps.DirectionsCache (DirectionsCache)
import qualified EulerHS.Language as L
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Logging (Log)
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.Maps.DirectionsCache as Queries

cacheDirectionsResponse :: CacheFlow m r => DirectionsCache -> m ()
cacheDirectionsResponse dirCache = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let dirKey = makeDirKey (dirCache.originHash) (dirCache.destHash) dirCache.slot
  Hedis.setExp dirKey dirCache expTime

makeDirKey :: Text -> Text -> Int -> Text
makeDirKey originHash destHash slot = pack "CachedQueries:Maps:origin-" <> originHash <> "-dest:" <> destHash <> "-slot:" <> (pack . show $ slot)

create :: (L.MonadFlow m, Log m) => DirectionsCache -> m ()
create = Queries.create

findRoute :: (CacheFlow m r, Esq.EsqDBFlow m r) => Text -> Text -> Int -> m (Maybe DirectionsCache)
findRoute originHash destHash slot =
  Hedis.safeGet (makeDirKey originHash destHash slot) >>= \case
    Just x -> return $ Just x
    Nothing -> do
      dbSearchRes <- Queries.findRoute originHash destHash slot
      case dbSearchRes of
        Just res -> do
          cacheDirectionsResponse res
          return dbSearchRes
        Nothing ->
          return Nothing
