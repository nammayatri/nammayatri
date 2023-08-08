{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Person.DisabilityType where

-- import Domain.Types.Person
import Domain.Types.Person.DisabilityType
-- import qualified EulerHS.Language as L
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
-- import Kernel.Types.Id
-- import Kernel.Types.Logging (Log)
-- import Kernel.Types.Time
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Person.DisabilityType as Queries

getAllDisabilities :: (CacheFlow m r, Esq.EsqDBFlow m r) => m [DisabilityType]
getAllDisabilities =
  Hedis.safeGet makeDisabilityTypeKey >>= \case
    Just a -> return a
    Nothing -> cacheDisabilityType /=<< Queries.getAllDisabilities

-- getAllDisabilities :: (L.MonadFlow m, Log m) => m [DisabilityType]
-- getAllDisabilities = findAllWithKV [Se.Is BeamDT.id $ Se.Not $ Se.Eq ""]

cacheDisabilityType :: CacheFlow m r => [DisabilityType] -> m ()
cacheDisabilityType disabilities = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let key = makeDisabilityTypeKey
  Hedis.setExp key disabilities expTime

makeDisabilityTypeKey :: Text
makeDisabilityTypeKey = "CachedQueries:Person:DisabilityType"
