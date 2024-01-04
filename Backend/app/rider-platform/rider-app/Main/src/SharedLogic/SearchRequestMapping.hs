{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SearchRequestMapping where

import qualified Domain.Types.Location as DL
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.SearchRequestMapping as DLM
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.SearchRequestMapping as QLM

buildPickUpLocationMapping :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DL.Location -> Text -> DLM.SearchRequestMappingTags -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> m DLM.SearchRequestMapping
buildPickUpLocationMapping locationId entityId tag merchantId merchantOperatingCityId = do
  id <- generateGUID
  let order = 0
  now <- getCurrentTime
  let version = "LATEST"
      createdAt = now
      updatedAt = now
  QLM.updatePastMappingVersions entityId order
  return DLM.SearchRequestMapping {..}

buildDropLocationMapping :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DL.Location -> Text -> DLM.SearchRequestMappingTags -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> m DLM.SearchRequestMapping
buildDropLocationMapping locationId entityId tag merchantId merchantOperatingCityId = do
  id <- generateGUID
  noOfEntries <- QLM.countOrders entityId
  let order = if noOfEntries == 0 then 1 else noOfEntries
  now <- getCurrentTime
  let version = "LATEST"
      createdAt = now
      updatedAt = now
  QLM.updatePastMappingVersions entityId order
  return DLM.SearchRequestMapping {..}
