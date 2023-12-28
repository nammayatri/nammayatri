{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DailyStats (updateByDriverId, findByDriverIdAndDate, create) where

import Data.Time (Day)
import qualified Domain.Types.DailyStats as DDS
import Domain.Types.Person (Person)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DailyStats as BeamDS

create :: MonadFlow m => DDS.DailyStats -> m ()
create = createWithKV

findByDriverIdAndDate :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Day -> m (Maybe DDS.DailyStats)
findByDriverIdAndDate (Id driverId) date =
  findOneWithKV
    [ Se.And
        [ Se.Is BeamDS.driverId $ Se.Eq driverId,
          Se.Is BeamDS.merchantLocalDate $ Se.Eq date
        ]
    ]

updateByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Money -> Int -> Meters -> Day -> m ()
updateByDriverId driverId totalEarnings numRides totalDistance merchantLocalDate = do
  updateOneWithKV
    [ Se.Set BeamDS.totalEarnings totalEarnings,
      Se.Set BeamDS.numRides numRides,
      Se.Set BeamDS.totalDistance totalDistance
    ]
    [ Se.And
        [ Se.Is BeamDS.driverId $ Se.Eq (driverId.getId),
          Se.Is BeamDS.merchantLocalDate $ Se.Eq merchantLocalDate
        ]
    ]

instance FromTType' BeamDS.DailyStats DDS.DailyStats where
  fromTType' BeamDS.DailyStatsT {..} = do
    pure $
      Just
        DDS.DailyStats
          { id = id,
            driverId = Id driverId,
            totalEarnings = totalEarnings,
            numRides = numRides,
            totalDistance = totalDistance,
            merchantLocalDate = merchantLocalDate
          }

instance ToTType' BeamDS.DailyStats DDS.DailyStats where
  toTType' DDS.DailyStats {..} = do
    BeamDS.DailyStatsT
      { BeamDS.id = id,
        BeamDS.driverId = getId driverId,
        BeamDS.totalEarnings = totalEarnings,
        BeamDS.numRides = numRides,
        BeamDS.totalDistance = totalDistance,
        BeamDS.merchantLocalDate = merchantLocalDate
      }
