{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.GoHomeConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.GoHomeConfig
import Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.GoHomeConfig as BeamGHC

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => GoHomeConfig -> m ()
create = createWithKV

findByMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> m (Maybe GoHomeConfig)
findByMerchantOpCityId (Id merchantOperatingCityId) = findOneWithKV [Se.Is BeamGHC.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

instance FromTType' BeamGHC.GoHomeConfig GoHomeConfig where
  fromTType' BeamGHC.GoHomeConfigT {..} = do
    pure $
      Just
        GoHomeConfig
          { merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            goHomeFromLocationRadius = mkDistanceWithDefaultMeters distanceUnit goHomeFromLocationRadiusValue goHomeFromLocationRadius,
            goHomeWayPointRadius = mkDistanceWithDefaultMeters distanceUnit goHomeWayPointRadiusValue goHomeWayPointRadius,
            goHomeBatchDelay = Seconds goHomeBatchDelay,
            ignoreWaypointsTill = mkDistanceWithDefaultMeters distanceUnit ignoreWaypointsTillValue $ Meters ignoreWaypointsTill,
            addStartWaypointAt = mkDistanceWithDefaultMeters distanceUnit addStartWaypointAtValue $ Meters addStartWaypointAt,
            newLocAllowedRadius = mkDistanceWithDefaultMeters distanceUnit newLocAllowedRadiusValue $ Meters newLocAllowedRadius,
            destRadius = mkDistanceWithDefaultMeters distanceUnit destRadiusValue $ Meters destRadiusMeters,
            ..
          }

instance ToTType' BeamGHC.GoHomeConfig GoHomeConfig where
  toTType' GoHomeConfig {..} = do
    let distanceUnit = Just destRadius.unit -- should be the same for all fields
    BeamGHC.GoHomeConfigT
      { BeamGHC.merchantId = getId merchantId,
        BeamGHC.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamGHC.goHomeFromLocationRadius = distanceToMeters goHomeFromLocationRadius,
        BeamGHC.goHomeWayPointRadius = distanceToMeters goHomeWayPointRadius,
        BeamGHC.goHomeFromLocationRadiusValue = Just $ distanceToHighPrecDistance distanceUnit goHomeFromLocationRadius,
        BeamGHC.goHomeWayPointRadiusValue = Just $ distanceToHighPrecDistance distanceUnit goHomeWayPointRadius,
        BeamGHC.goHomeBatchDelay = getSeconds goHomeBatchDelay,
        BeamGHC.ignoreWaypointsTill = getMeters $ distanceToMeters ignoreWaypointsTill,
        BeamGHC.addStartWaypointAt = getMeters $ distanceToMeters addStartWaypointAt,
        BeamGHC.newLocAllowedRadius = getMeters $ distanceToMeters newLocAllowedRadius,
        BeamGHC.ignoreWaypointsTillValue = Just $ distanceToHighPrecDistance distanceUnit ignoreWaypointsTill,
        BeamGHC.addStartWaypointAtValue = Just $ distanceToHighPrecDistance distanceUnit addStartWaypointAt,
        BeamGHC.newLocAllowedRadiusValue = Just $ distanceToHighPrecDistance distanceUnit newLocAllowedRadius,
        BeamGHC.destRadiusMeters = getMeters $ distanceToMeters destRadius,
        BeamGHC.destRadiusValue = Just $ distanceToHighPrecDistance distanceUnit destRadius,
        ..
      }
