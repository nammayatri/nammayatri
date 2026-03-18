{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PayoutMethodExtra where

import qualified Domain.Types.PayoutMethod as Domain
import Domain.Types.Person (Person)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PayoutMethod as Beam
import Storage.Queries.OrphanInstances.PayoutMethod ()

-- Extra code goes here --

-- | M8 fix: Bulk update to unset isPrimary for all methods belonging to a driver.
-- Uses a single UPDATE query instead of N+1 individual updates.
unsetAllPrimaryByDriverId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  UTCTime ->
  m ()
unsetAllPrimaryByDriverId driverId now =
  updateWithKV
    [ Se.Set Beam.isPrimary False,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (getId driverId),
          Se.Is Beam.isPrimary $ Se.Eq True
        ]
    ]
