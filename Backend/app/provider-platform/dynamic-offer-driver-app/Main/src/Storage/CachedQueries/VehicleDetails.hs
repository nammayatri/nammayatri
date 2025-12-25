{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.VehicleDetails where

import qualified Data.Text
import Domain.Types.VehicleDetails
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Storage.Queries.VehicleDetails as QVehicleDetails
import qualified Storage.Queries.VehicleDetailsExtra as QVehicleDetailsExtra

findAllVehicleDetails :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [VehicleDetails]
findAllVehicleDetails = do
  Hedis.safeGet makeVehicleDetailsKey >>= \case
    Just a -> return a
    Nothing -> findAndCache
  where
    findAndCache = cacheVehicleDetails /=<< QVehicleDetailsExtra.findAllVehicleDetails

cacheVehicleDetails :: (CacheFlow m r) => [VehicleDetails] -> m ()
cacheVehicleDetails vehicleDetails = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp makeVehicleDetailsKey vehicleDetails expTime

makeVehicleDetailsKey :: Text
makeVehicleDetailsKey = "CachedQueries:ValueAddNP:VehicleDetails"

findByMakeAndYear :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> Maybe Int -> m ([Domain.Types.VehicleDetails.VehicleDetails]))
findByMakeAndYear make year = do
  Hedis.safeGet (makeVehicleDetailsMakeYearKey make year) >>= \case
    Just a -> return a
    Nothing -> findAndCache
  where
    findAndCache = cacheVehicleDetailsMakeYear make year /=<< QVehicleDetails.findByMakeAndYear make year

cacheVehicleDetailsMakeYear :: (CacheFlow m r) => Text -> Maybe Int -> [Domain.Types.VehicleDetails.VehicleDetails] -> m ()
cacheVehicleDetailsMakeYear make year vehicleDetails = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeVehicleDetailsMakeYearKey make year) vehicleDetails expTime

makeVehicleDetailsMakeYearKey :: Text -> Maybe Int -> Text
makeVehicleDetailsMakeYearKey make year = "CachedQueries:ValueAddNP:VehicleDetails:Make-" <> make <> ":Year-" <> (maybe "All" show year)

findByMakeAndModelAndYear :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> Data.Text.Text -> Maybe Int -> m (Maybe Domain.Types.VehicleDetails.VehicleDetails))
findByMakeAndModelAndYear make model year = do
  Hedis.safeGet (makeVehicleDetailsMakeModelYearKey make model year) >>= \case
    Just a -> return a
    Nothing -> findAndCache
  where
    findAndCache = cacheVehicleDetailsMakeModelYear make model year /=<< QVehicleDetails.findByMakeAndModelAndYear make model year

cacheVehicleDetailsMakeModelYear :: (CacheFlow m r) => Text -> Text -> Maybe Int -> Maybe Domain.Types.VehicleDetails.VehicleDetails -> m ()
cacheVehicleDetailsMakeModelYear make model year vehicleDetails = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeVehicleDetailsMakeModelYearKey make model year) vehicleDetails expTime

makeVehicleDetailsMakeModelYearKey :: Text -> Text -> Maybe Int -> Text
makeVehicleDetailsMakeModelYearKey make model year = "CachedQueries:ValueAddNP:VehicleDetails:Make-" <> make <> ":Model-" <> model <> ":Year-" <> (maybe "All" show year)
