{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.LocationMapping where

import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.LocationMapping as QLM

buildPickUpLocationMapping :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DL.Location -> Text -> DLM.LocationMappingTags -> m DLM.LocationMapping
buildPickUpLocationMapping locationId entityId tag = do
  id <- generateGUID
  let order = 0
  QLM.updatePastMappingVersions entityId order
  let version = "LATEST"
  return DLM.LocationMapping {..}

buildDropLocationMapping :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DL.Location -> Text -> DLM.LocationMappingTags -> m DLM.LocationMapping
buildDropLocationMapping locationId entityId tag = do
  id <- generateGUID
  noOfEntries <- QLM.countOrders entityId
  let order = if noOfEntries == 0 then 1 else noOfEntries
  QLM.updatePastMappingVersions entityId order
  let version = "LATEST"
  return DLM.LocationMapping {..}
