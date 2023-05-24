{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Person.PersonFlowStatus where

import Domain.Types.Person
import Domain.Types.Person.PersonFlowStatus
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Esqueleto.DeletedEntity as EsqDE
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Person.PersonFlowStatus as Queries

create :: PersonFlowStatus -> Esq.SqlDB ()
create = Queries.create

getStatus :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id Person -> m (Maybe FlowStatus)
getStatus personId =
  Hedis.safeGet (makeFlowStatusKey personId) >>= \case
    Just a -> return a
    Nothing -> flip whenJust (cachedStatus personId) /=<< Queries.getStatus personId

updateStatus :: Id Person -> FlowStatus -> Esq.SqlDB ()
updateStatus = Queries.updateStatus

deleteByPersonId :: EsqDE.DeletedBy -> Id Person -> Esq.SqlDB ()
deleteByPersonId = Queries.deleteByPersonId

updateToIdleMultiple :: [Id Person] -> UTCTime -> Esq.SqlDB ()
updateToIdleMultiple = Queries.updateToIdleMultiple

cachedStatus :: CacheFlow m r => Id Person -> FlowStatus -> m ()
cachedStatus personId flowStatus = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let personIdKey = makeFlowStatusKey personId
  Hedis.setExp personIdKey flowStatus expTime

makeFlowStatusKey :: Id Person -> Text
makeFlowStatusKey personId = "CachedQueries:Person:FlowStatus-" <> personId.getId

clearCache :: CacheFlow m r => Id Person -> m ()
clearCache personId = do
  Hedis.del (makeFlowStatusKey personId)
