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

findAllVehicleDetails :: KvDbFlow m r => m [VehicleDetails]
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

findByMake :: KvDbFlow m r => (Data.Text.Text -> m ([Domain.Types.VehicleDetails.VehicleDetails]))
findByMake make = do
  Hedis.safeGet (makeVehicleDetailsMakeKey make) >>= \case
    Just a -> return a
    Nothing -> findAndCache
  where
    findAndCache = cacheVehicleDetailsMake make /=<< QVehicleDetails.findByMake make

cacheVehicleDetailsMake :: (CacheFlow m r) => Text -> [Domain.Types.VehicleDetails.VehicleDetails] -> m ()
cacheVehicleDetailsMake make vehicleDetails = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeVehicleDetailsMakeKey make) vehicleDetails expTime

makeVehicleDetailsMakeKey :: Text -> Text
makeVehicleDetailsMakeKey make = "CachedQueries:ValueAddNP:VehicleDetails:Make-" <> make

findByMakeAndModel :: KvDbFlow m r => (Data.Text.Text -> Data.Text.Text -> m (Maybe Domain.Types.VehicleDetails.VehicleDetails))
findByMakeAndModel make model = do
  Hedis.safeGet (makeVehicleDetailsMakeModelKey make model) >>= \case
    Just a -> return a
    Nothing -> findAndCache
  where
    findAndCache = cacheVehicleDetailsMakeModel make model /=<< QVehicleDetails.findByMakeAndModel make model

cacheVehicleDetailsMakeModel :: (CacheFlow m r) => Text -> Text -> Maybe Domain.Types.VehicleDetails.VehicleDetails -> m ()
cacheVehicleDetailsMakeModel make model vehicleDetails = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeVehicleDetailsMakeModelKey make model) vehicleDetails expTime

makeVehicleDetailsMakeModelKey :: Text -> Text -> Text
makeVehicleDetailsMakeModelKey make model = "CachedQueries:ValueAddNP:VehicleDetails:Make-" <> make <> ":Model-" <> model
