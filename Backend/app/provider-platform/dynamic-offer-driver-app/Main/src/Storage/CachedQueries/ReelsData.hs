{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.ReelsData where

import qualified Domain.Types.MerchantOperatingCity as DMMOC
import qualified Domain.Types.ReelsData as DTRD
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Queries.ReelsData as SQReels

findAllByMerchantOpCityIdLanguageAndKey :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id DMMOC.MerchantOperatingCity -> Language -> Text -> m [DTRD.ReelsData]
findAllByMerchantOpCityIdLanguageAndKey merchantOpCityId language key =
  Hedis.safeGet (makeReelsDataKeyByMerchantIOpCityIdLanguageAndKey merchantOpCityId language key) >>= \case
    Just a -> pure a
    Nothing -> cacheByMerchantOpCityIdLanguageAndKey merchantOpCityId language key /=<< SQReels.findAllByMerchantOpCityIdLanguageAndKey merchantOpCityId language key

cacheByMerchantOpCityIdLanguageAndKey :: (CacheFlow m r) => Id DMMOC.MerchantOperatingCity -> Language -> Text -> [DTRD.ReelsData] -> m ()
cacheByMerchantOpCityIdLanguageAndKey merchantOpCityId language key reelsData = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeReelsDataKeyByMerchantIOpCityIdLanguageAndKey merchantOpCityId language key) reelsData expTime

makeReelsDataKeyByMerchantIOpCityIdLanguageAndKey :: Id DMMOC.MerchantOperatingCity -> Language -> Text -> Text
makeReelsDataKeyByMerchantIOpCityIdLanguageAndKey merchantOpCityId language key = "driver-offer:CachedQueries:ReelsData:merchantOperatingCityId-" <> merchantOpCityId.getId <> ":language-" <> show language <> ":key-" <> key
