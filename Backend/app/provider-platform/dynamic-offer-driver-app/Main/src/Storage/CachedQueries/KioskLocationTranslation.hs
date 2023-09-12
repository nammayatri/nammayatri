{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.KioskLocationTranslation where

import Domain.Types.KioskLocation
import Domain.Types.KioskLocationTranslation
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.KioskLocationTranslation as Queries

findByKioskLocationIdAndLanguage :: (CacheFlow m r, MonadFlow m) => Id KioskLocation -> Language -> m (Maybe KioskLocationTranslation)
findByKioskLocationIdAndLanguage (Id kioskLocationId) language =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeKioskLocationAndLanguageKey (Id kioskLocationId) language) >>= \case
    Just a -> pure a
    Nothing -> cacheByKioskLocationIdAndLanguage (Id kioskLocationId) language /=<< Queries.findByKioskLocationIdAndLanguage (Id kioskLocationId) language

cacheByKioskLocationIdAndLanguage :: (CacheFlow m r) => Id KioskLocation -> Language -> Maybe KioskLocationTranslation -> m ()
cacheByKioskLocationIdAndLanguage (Id kioskLocationId) language kioskLocationTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeKioskLocationAndLanguageKey (Id kioskLocationId) language) kioskLocationTranslation expTime

makeKioskLocationAndLanguageKey :: Id KioskLocation -> Language -> Text
makeKioskLocationAndLanguageKey id language = "driver-offer:CachedQueries:KioskLocationTranslation-" <> id.getId <> ":Language-" <> show language
