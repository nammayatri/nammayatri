{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PayoutInstanceCalculationExtra where

import qualified Domain.Types.PayoutInstanceCalculation as DPIC
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common as Common
import Kernel.Types.Error
import qualified Kernel.Types.Id as KId
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PayoutInstanceCalculation as Beam
import Storage.Queries.OrphanInstances.PayoutInstanceCalculation

-- Extra code goes here --

-- Find existing instance by fromVendorId and toVendorId within time range
findByFromAndToVendorId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  UTCTime ->
  UTCTime ->
  m (Maybe DPIC.PayoutInstanceCalculation)
findByFromAndToVendorId fromVendorId toVendorId startTime endTime =
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.fromVendorId $ Se.Eq fromVendorId,
          Se.Is Beam.toVendorId $ Se.Eq toVendorId,
          Se.Is Beam.startTime $ Se.Eq startTime,
          Se.Is Beam.endTime $ Se.Eq endTime
        ]
    ]

-- Update instance balance by setting to a new value
updateInstanceBalance ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  KId.Id DPIC.PayoutInstanceCalculation ->
  Common.HighPrecMoney ->
  UTCTime ->
  m ()
updateInstanceBalance instanceId newBalance now = do
  updateWithKV
    [ Se.Set Beam.instanceBalance newBalance,
      Se.Set Beam.updatedAt now
    ]
    [ Se.Is Beam.id $ Se.Eq (KId.getId instanceId)
    ]

-- Update instance balance by fromVendorId, toVendorId, and time range
updateInstanceBalanceByVendorIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  UTCTime ->
  UTCTime ->
  Common.HighPrecMoney ->
  UTCTime ->
  m ()
updateInstanceBalanceByVendorIds fromVendorId toVendorId startTime endTime newBalance now = do
  updateWithKV
    [ Se.Set Beam.instanceBalance newBalance,
      Se.Set Beam.updatedAt now
    ]
    [ Se.And
        [ Se.Is Beam.fromVendorId $ Se.Eq fromVendorId,
          Se.Is Beam.toVendorId $ Se.Eq toVendorId,
          Se.Is Beam.startTime $ Se.Eq startTime,
          Se.Is Beam.endTime $ Se.Eq endTime
        ]
    ]
