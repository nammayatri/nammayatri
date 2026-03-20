{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.JourneyLeg
  ( findJourneyIdByLegSearchId,
    clearCache,
  )
where

import Domain.Types.Journey as Journey
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.JourneyLeg as Queries

findJourneyIdByLegSearchId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  m (Maybe (Id Journey.Journey))
findJourneyIdByLegSearchId legSearchId = IM.withInMemCache ["JourneyIdByLegSearchId", legSearchId] 3600 $ do
  Hedis.safeGet cacheKey >>= \case
    Just a -> pure a
    Nothing -> do
      journeyLeg <- Queries.findByLegSearchId (Just legSearchId)
      whenJust journeyLeg $ \leg -> do
        expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
        Hedis.setExp cacheKey leg.journeyId expTime
      return (journeyLeg <&> (.journeyId))
  where
    cacheKey = "CachedQueries:JourneyLeg:journeyIdByLegSearchId:" <> legSearchId

clearCache :: (CacheFlow m r) => Text -> m ()
clearCache legSearchId = do
  Hedis.del $ "CachedQueries:JourneyLeg:journeyIdByLegSearchId:" <> legSearchId
