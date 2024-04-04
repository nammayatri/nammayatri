{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DailyStatsExtra where

import Data.Time (Day)
import Domain.Types.DailyStats
import qualified Domain.Types.Person as SP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DailyStats as Beam
import Storage.Queries.OrphanInstances.DailyStats

-- Extra code goes here --

findAllInRangeByDriverId :: KvDbFlow m r => Id SP.Person -> Day -> Day -> m [DailyStats]
findAllInRangeByDriverId (Id driverId) from to = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq driverId,
          Se.Is Beam.merchantLocalDate $ Se.GreaterThanOrEq from,
          Se.Is Beam.merchantLocalDate $ Se.LessThan to
        ]
    ]
