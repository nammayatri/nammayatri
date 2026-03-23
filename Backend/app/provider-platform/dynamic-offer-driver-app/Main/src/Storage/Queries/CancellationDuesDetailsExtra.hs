{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CancellationDuesDetailsExtra where

import qualified Domain.Types.CancellationDuesDetails as DCDD
import qualified Domain.Types.RiderDetails as DRD
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationDuesDetails as Beam
import Storage.Queries.OrphanInstances.CancellationDuesDetails

updateAllPendingToPaidByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id.Id DRD.RiderDetails -> m ()
updateAllPendingToPaidByRiderId riderId = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.paymentStatus DCDD.PAID, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (Id.getId riderId),
          Se.Is Beam.paymentStatus $ Se.Eq DCDD.PENDING
        ]
    ]

findAllPendingByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id.Id DRD.RiderDetails -> m [DCDD.CancellationDuesDetails]
findAllPendingByRiderId riderId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (Id.getId riderId),
          Se.Is Beam.paymentStatus $ Se.Eq DCDD.PENDING
        ]
    ]

updateStatusByIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DCDD.CancellationDuesPaymentStatus -> [Id.Id DCDD.CancellationDuesDetails] -> m ()
updateStatusByIds status ids = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.paymentStatus status, Se.Set Beam.updatedAt _now]
    [Se.Is Beam.id $ Se.In (map Id.getId ids)]
