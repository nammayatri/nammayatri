{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PayoutInstanceCalculation (module Storage.Queries.PayoutInstanceCalculation, module ReExport) where

import qualified Domain.Types.PayoutInstanceCalculation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PayoutInstanceCalculation as Beam
import Storage.Queries.PayoutInstanceCalculationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutInstanceCalculation.PayoutInstanceCalculation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PayoutInstanceCalculation.PayoutInstanceCalculation] -> m ())
createMany = traverse_ create

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PayoutInstanceCalculation.PayoutInstanceCalculation -> m (Maybe Domain.Types.PayoutInstanceCalculation.PayoutInstanceCalculation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutInstanceCalculation.PayoutInstanceCalculation -> m ())
updateByPrimaryKey (Domain.Types.PayoutInstanceCalculation.PayoutInstanceCalculation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.endTime endTime,
      Se.Set Beam.fromVendorId fromVendorId,
      Se.Set Beam.instanceBalance instanceBalance,
      Se.Set Beam.runningBalance runningBalance,
      Se.Set Beam.settlementDate settlementDate,
      Se.Set Beam.settlementMode settlementMode,
      Se.Set Beam.startTime startTime,
      Se.Set Beam.status status,
      Se.Set Beam.toVendorId toVendorId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
