{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.ConfigMapping
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant.ConfigMapping
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as ID
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.ConfigMapping as BeamCM

getConfigMapId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> Meters -> Maybe Variant -> TimeOfDay -> Text -> m (Maybe ConfigMapping)
getConfigMapId (Id merchantOperatingCityId) distance varType currTime tableName =
  --CMTODO: Convert UTC Time to TimeOfDay (IST) then do Cache Queries then start handling the tables (also remember the dashboard update and creation apis.)
  findAllWithKV
    [ Se.And
        [ Se.Is BeamCM.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is BeamCM.startDistance $ Se.LessThanOrEq distance, --CMTODO: Tell PMs it functions like [,)
          Se.Is BeamCM.endDistance $ Se.GreaterThan distance,
          Se.Is BeamCM.varType $ Se.Eq varType,
          Se.Is BeamCM.startTime $ Se.GreaterThanOrEq currTime,
          Se.Is BeamCM.endTime $ Se.LessThan currTime, --CMTODO: Ask everyone to use 24:00:00 instead of 00:00:00 for midnight end. -> time intervals are like [,)
          Se.Is BeamCM.tableName $ Se.Eq tableName
        ]
    ]
    <&> listToMaybe

instance FromTType' BeamCM.ConfigMapping ConfigMapping where
  fromTType' BeamCM.ConfigMappingT {..} = do
    pure $
      Just
        ConfigMapping
          { merchantId = ID.Id merchantId,
            merchantOperatingCityId = ID.Id merchantOperatingCityId,
            configMapId = ID.Id configMapId,
            ..
          }

instance ToTType' BeamCM.ConfigMapping ConfigMapping where
  toTType' ConfigMapping {..} = do
    BeamCM.ConfigMappingT
      { BeamCM.merchantId = getId merchantId,
        BeamCM.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamCM.configMapId = getId configMapId,
        ..
      }
