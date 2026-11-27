{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantMessage
  ( create,
    findAllByMerchantOpCityId,
    findAllByMerchantOpCityIdInRideFlow,
    findByMerchantOpCityIdAndMessageKeyVehicleCategory,
    findByMerchantOpCityIdAndMessageKeyLanguageVehicleCategory,
    clearCache,
    clearCacheById,
  )
where

import Domain.Types.MerchantMessage
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.MerchantMessage as Queries
import qualified Tools.DynamicLogic as DynamicLogic

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => MerchantMessage -> m ()
create = Queries.create

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [MerchantMessage]
findAllByMerchantOpCityId id mbConfigVersionMap =
  DynamicLogic.findAllConfigs (cast id) (LYT.DRIVER_CONFIG LYT.MerchantMessage) mbConfigVersionMap Nothing (Queries.findAllByMerchantOpCityId id)

findAllByMerchantOpCityIdInRideFlow :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> [LYT.ConfigVersionMap] -> m [MerchantMessage]
findAllByMerchantOpCityIdInRideFlow id configVersionMap =
  findAllByMerchantOpCityId id (Just configVersionMap)

makeMerchantOpCityIdAndMessageKey :: Id MerchantOperatingCity -> MessageKey -> Maybe DVC.VehicleCategory -> Text
makeMerchantOpCityIdAndMessageKey id messageKey mbVehicleCategory = "CachedQueries:MerchantMessage:MerchantOperatingCityId-" <> id.getId <> ":MessageKey-" <> show messageKey <> maybe "" ((":VehCat-" <>) . show) mbVehicleCategory <> ":AllLanguages"

-- | The English row: what every sender that does not know its recipient's language reads.
findByMerchantOpCityIdAndMessageKeyVehicleCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> MessageKey -> Maybe DVC.VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m (Maybe MerchantMessage)
findByMerchantOpCityIdAndMessageKeyVehicleCategory id messageKey vehicleCategory mbConfigVersionMap =
  findByMerchantOpCityIdAndMessageKeyLanguageVehicleCategory id messageKey Nothing vehicleCategory mbConfigVersionMap

-- | The recipient's language, then English; the vehicle category's row, then the general one.
-- The general rows are only read when the vehicle category has no row in the preferred language.
-- Each cached list holds every language, so a language with no row is a cached answer too; a key
-- with no row at all is not kept, so a row added outside the dashboard is read on the next call.
findByMerchantOpCityIdAndMessageKeyLanguageVehicleCategory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> MessageKey -> Maybe Language -> Maybe DVC.VehicleCategory -> Maybe [LYT.ConfigVersionMap] -> m (Maybe MerchantMessage)
findByMerchantOpCityIdAndMessageKeyLanguageVehicleCategory id messageKey mbLanguage vehicleCategory mbConfigVersionMap = do
  vehicleCategoryRows <- maybe (pure []) (findAllByVehicleCategory . Just) vehicleCategory
  case find ((== preferred) . (.language)) vehicleCategoryRows of
    Just mm -> pure (Just mm)
    Nothing -> do
      generalRows <- findAllByVehicleCategory Nothing
      pure $ listToMaybe [mm | language <- languages, mm <- vehicleCategoryRows <> generalRows, mm.language == language]
  where
    preferred = fromMaybe ENGLISH mbLanguage
    languages = if preferred == ENGLISH then [ENGLISH] else [preferred, ENGLISH]
    findAllByVehicleCategory mbVehicleCategory = do
      rows <-
        DynamicLogic.findAllConfigsWithCacheKey
          (cast id)
          (LYT.DRIVER_CONFIG LYT.MerchantMessage)
          mbConfigVersionMap
          Nothing
          (Queries.findAllByMerchantOpCityIdAndMessageKeyAndVehicleCategory id messageKey mbVehicleCategory)
          (makeMerchantOpCityIdAndMessageKey id messageKey mbVehicleCategory)
      when (null rows) $ clearCache id messageKey mbVehicleCategory
      pure rows

clearCache :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> MessageKey -> Maybe DVC.VehicleCategory -> m ()
clearCache merchantOpCityId messageKey mbVehicleCategory =
  DynamicLogic.clearConfigCacheWithPrefix
    (makeMerchantOpCityIdAndMessageKey merchantOpCityId messageKey mbVehicleCategory)
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.MerchantMessage)
    Nothing

clearCacheById :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m ()
clearCacheById merchantOpCityId =
  DynamicLogic.clearConfigCache
    (cast merchantOpCityId)
    (LYT.DRIVER_CONFIG LYT.MerchantMessage)
    Nothing
