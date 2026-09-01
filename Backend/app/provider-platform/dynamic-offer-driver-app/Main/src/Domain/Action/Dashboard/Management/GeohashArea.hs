{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Dashboard entry points for the geohash-area label table. Both upsert paths
-- (map-selection JSON, CSV upload) delegate to the same core in
-- 'SharedLogic.GeohashAreaUpsert' -- this module only handles request parsing and
-- merchant/city resolution, which is identical across all three endpoints.
module Domain.Action.Dashboard.Management.GeohashArea
  ( getGeohashAreaList,
    postGeohashAreaUpsert,
    postGeohashAreaUpsertCsv,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.GeohashArea as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.GeohashAreaUpsert as SLGA
import qualified SharedLogic.Merchant as SLM
import qualified Storage.CachedQueries.GeohashArea as CQGA
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

getGeohashAreaList :: ShortId DM.Merchant -> Context.City -> Flow [Common.GeohashAreaItem]
getGeohashAreaList merchantShortId opCity = do
  merchantOpCity <- findMerchantOpCity merchantShortId opCity
  areas <- CQGA.findAllByMerchantOperatingCity merchantOpCity.id
  pure [Common.GeohashAreaItem {geohash = a.geohash, areaName = a.areaName} | a <- areas]

postGeohashAreaUpsert :: ShortId DM.Merchant -> Context.City -> Common.GeohashAreaBulkUpsertReq -> Flow APISuccess
postGeohashAreaUpsert merchantShortId opCity req = do
  merchantOpCity <- findMerchantOpCity merchantShortId opCity
  result <- SLGA.upsertGeohashAreas merchantOpCity req.areas
  CQGA.clearCacheByMerchantOperatingCity merchantOpCity.id
  pure result

postGeohashAreaUpsertCsv :: ShortId DM.Merchant -> Context.City -> Common.GeohashAreaCsvReq -> Flow APISuccess
postGeohashAreaUpsertCsv merchantShortId opCity req = do
  merchantOpCity <- findMerchantOpCity merchantShortId opCity
  result <- SLGA.upsertGeohashAreasFromCsv merchantOpCity req.file
  CQGA.clearCacheByMerchantOperatingCity merchantOpCity.id
  pure result

findMerchantOpCity :: ShortId DM.Merchant -> Context.City -> Flow DMOC.MerchantOperatingCity
findMerchantOpCity merchantShortId opCity = do
  merchant <- SLM.findMerchantByShortId merchantShortId
  CQMOC.findByMerchantIdAndCity merchant.id opCity
    >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
