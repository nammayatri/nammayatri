{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Mandate (module Storage.Queries.Mandate, module ReExport) where

import qualified Domain.Types.Mandate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Mandate as Beam
import Storage.Queries.MandateExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Mandate.Mandate -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Mandate.Mandate] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Mandate.Mandate -> m (Maybe Domain.Types.Mandate.Mandate))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Mandate.Mandate -> Domain.Types.Mandate.MandateStatus -> m (Maybe Domain.Types.Mandate.Mandate))
findByStatus id status = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.status $ Se.Eq status]]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Mandate.MandateStatus -> Kernel.Types.Id.Id Domain.Types.Mandate.Mandate -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Mandate.Mandate -> m (Maybe Domain.Types.Mandate.Mandate))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Mandate.Mandate -> m ())
updateByPrimaryKey (Domain.Types.Mandate.Mandate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.currency (Kernel.Prelude.Just currency),
      Se.Set Beam.endDate endDate,
      Se.Set Beam.mandatePaymentFlow mandatePaymentFlow,
      Se.Set Beam.maxAmount maxAmount,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.payerApp payerApp,
      Se.Set Beam.payerAppName payerAppName,
      Se.Set Beam.payerVpa payerVpa,
      Se.Set Beam.startDate startDate,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
