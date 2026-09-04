{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Bulk create/update of geohash -> area-name labels for a city. One core function
-- ('upsertGeohashAreas') shared by both dashboard entry points -- the map-selection
-- JSON endpoint and the CSV upload both flatten down to the same [GeohashAreaItem]
-- before reaching here, so there is exactly one place that decides what "upsert"
-- means for this table.
--
-- Best-effort by design: a malformed row (empty geohash/areaName) is skipped and
-- the rest of the batch still applies. No per-row reporting back to the caller --
-- if something didn't land, re-check via the list endpoint.
module SharedLogic.GeohashAreaUpsert
  ( GeohashAreaCSVRow (..),
    upsertGeohashAreas,
    upsertGeohashAreasFromCsv,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.GeohashArea as Common
import Data.Csv
import qualified Domain.Types.GeohashArea as DGA
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Utils.Common
import qualified Storage.Queries.GeohashArea as QGeohashArea
import Tools.Csv (cleanField, readCsv)
import Tools.Error

data GeohashAreaCSVRow = GeohashAreaCSVRow
  { geohash :: Text,
    areaName :: Text
  }
  deriving (Generic, FromNamedRecord)

upsertGeohashAreasFromCsv :: DMOC.MerchantOperatingCity -> FilePath -> Flow APISuccess
upsertGeohashAreasFromCsv merchantOpCity csvFile = do
  rows <- readCsv csvFile (\_idx (row :: GeohashAreaCSVRow) -> pure (Common.GeohashAreaItem {geohash = row.geohash, areaName = row.areaName}))
  upsertGeohashAreas merchantOpCity rows

upsertGeohashAreas :: DMOC.MerchantOperatingCity -> [Common.GeohashAreaItem] -> Flow APISuccess
upsertGeohashAreas merchantOpCity items = do
  forM_ items $ \item ->
    void $ withTryCatch "upsertGeohashArea" (upsertOne item)
  pure Success
  where
    upsertOne item = do
      geohash <- cleanField item.geohash & fromMaybeM (InvalidRequest $ "Invalid geohash: " <> show item.geohash)
      areaName <- cleanField item.areaName & fromMaybeM (InvalidRequest $ "Invalid areaName: " <> show item.areaName)
      mbExisting <- QGeohashArea.findByMerchantOperatingCityAndGeohash (Just merchantOpCity.id) geohash
      now <- getCurrentTime
      case mbExisting of
        Just existing -> QGeohashArea.updateByPrimaryKey existing {DGA.areaName = areaName, DGA.updatedAt = now}
        Nothing -> do
          newId <- generateGUID
          QGeohashArea.create
            DGA.GeohashArea
              { DGA.id = newId,
                DGA.geohash = geohash,
                DGA.areaName = areaName,
                DGA.merchantId = Just merchantOpCity.merchantId,
                DGA.merchantOperatingCityId = Just merchantOpCity.id,
                DGA.createdAt = now,
                DGA.updatedAt = now
              }
