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
import Domain.Types.Merchant
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.GoHomeConfig as BeamGHC

findByMerchantId :: (MonadFlow m) => Id Merchant -> m (Maybe GoHomeConfig)
findByMerchantId (Id merchantId) = findOneWithKV [Se.Is BeamGHC.merchantId $ Se.Eq merchantId]

instance FromTType' BeamGHC.GoHomeConfig GoHomeConfig where
  fromTType' BeamGHC.GoHomeConfigT {..} = do
    pure $
      Just
        GoHomeConfig
          { merchantId = Id merchantId,
            goHomeFromLocationRadius = goHomeFromLocationRadius,
            goHomeWayPointRadius = goHomeWayPointRadius,
            goHomeBatchDelay = Seconds goHomeBatchDelay,
            ignoreWaypointsTill = Meters ignoreWaypointsTill,
            addStartWaypointAt = Meters addStartWaypointAt,
            ..
          }

instance ToTType' BeamGHC.GoHomeConfig GoHomeConfig where
  toTType' GoHomeConfig {..} = do
    BeamGHC.GoHomeConfigT
      { BeamGHC.merchantId = getId merchantId,
        BeamGHC.goHomeFromLocationRadius = goHomeFromLocationRadius,
        BeamGHC.goHomeWayPointRadius = goHomeWayPointRadius,
        BeamGHC.goHomeBatchDelay = getSeconds goHomeBatchDelay,
        BeamGHC.ignoreWaypointsTill = getMeters ignoreWaypointsTill,
        BeamGHC.addStartWaypointAt = getMeters addStartWaypointAt,
        ..
      }
