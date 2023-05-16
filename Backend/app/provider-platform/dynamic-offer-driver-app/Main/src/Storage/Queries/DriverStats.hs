{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverStats where

import Domain.Types.DriverStats
import Domain.Types.Person (Driver)
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.DriverStats as BeamDS
import Storage.Tabular.DriverStats

createInitialDriverStats :: Id Driver -> SqlDB ()
createInitialDriverStats driverId = do
  now <- getCurrentTime
  Esq.create $
    DriverStats
      { driverId = driverId,
        idleSince = now
      }

getTopDriversByIdleTime :: Transactionable m => Int -> [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime count_ ids =
  Esq.findAll $ do
    driverStats <- from $ table @DriverStatsT
    where_ $ driverStats ^. DriverStatsDriverId `in_` valList (toKey . cast <$> ids)
    orderBy [asc $ driverStats ^. DriverStatsIdleSince]
    limit $ fromIntegral count_
    return $ driverStats ^. DriverStatsTId

updateIdleTime :: Id Driver -> SqlDB ()
updateIdleTime driverId = updateIdleTimes [driverId]

updateIdleTimes :: [Id Driver] -> SqlDB ()
updateIdleTimes driverIds = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverStatsIdleSince =. val now
      ]
    where_ $ tbl ^. DriverStatsDriverId `in_` valList (toKey . cast <$> driverIds)

fetchAll :: Transactionable m => m [DriverStats]
fetchAll = Esq.findAll $ from $ table @DriverStatsT

deleteById :: Id Driver -> SqlDB ()
deleteById = Esq.deleteByKey @DriverStatsT

transformBeamDriverStatsToDomain :: BeamDS.DriverStats -> DriverStats
transformBeamDriverStatsToDomain BeamDS.DriverStatsT {..} = do
  DriverStats
    { driverId = Id driverId,
      idleSince = idleSince
    }

transformDomainDriverStatsToBeam :: DriverStats -> BeamDS.DriverStats
transformDomainDriverStatsToBeam DriverStats {..} =
  BeamDS.defaultDriverStats
    { BeamDS.driverId = getId driverId,
      BeamDS.idleSince = idleSince
    }
