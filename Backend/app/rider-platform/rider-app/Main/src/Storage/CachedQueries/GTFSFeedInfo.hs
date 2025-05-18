{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.GTFSFeedInfo
  ( findByVehicleTypeAndCity,
  )
where

import BecknV2.FRFS.Enums (VehicleCategory (..))
import Data.Text as Text
import Domain.Types.GTFSFeedInfo
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude as P
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.GTFSFeedInfo as Queries
import Tools.Error (GTFSError (..))

findByVehicleTypeAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  VehicleCategory ->
  Id MerchantOperatingCity ->
  Id Merchant ->
  m GTFSFeedInfo
findByVehicleTypeAndCity vehicleType merchantOpId merchantId = do
  Hedis.safeGet (gtfsFeedInfoKey merchantOpId merchantId vehicleType) >>= \case
    Just a -> pure a
    Nothing -> do
      gtfsFeedInfo <- Queries.findByVehicleTypeAndCity vehicleType merchantId merchantOpId >>= fromMaybeM (GTFSFeedInfoNotFound (Text.pack $ show vehicleType) merchantOpId.getId)
      cacheGtfsFeedInfo gtfsFeedInfo
      return gtfsFeedInfo

cacheGtfsFeedInfo :: (CacheFlow m r, MonadFlow m) => GTFSFeedInfo -> m ()
cacheGtfsFeedInfo gtfsFeedInfo = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = gtfsFeedInfoKey gtfsFeedInfo.merchantOperatingCityId gtfsFeedInfo.merchantId gtfsFeedInfo.vehicleType
  Hedis.setExp idKey gtfsFeedInfo expTime

gtfsFeedInfoKey :: Id MerchantOperatingCity -> Id Merchant -> VehicleCategory -> Text
gtfsFeedInfoKey merchantOpId merchantId vehicleType = "gtfsfeedinfo:" <> merchantOpId.getId <> ":" <> merchantId.getId <> ":" <> show vehicleType
