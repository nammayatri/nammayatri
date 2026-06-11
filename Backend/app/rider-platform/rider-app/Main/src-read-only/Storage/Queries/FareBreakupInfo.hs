{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareBreakupInfo (module Storage.Queries.FareBreakupInfo, module ReExport) where

import qualified Domain.Types.FareBreakup
import qualified Domain.Types.FareBreakupInfo
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Text
import qualified Sequelize as Se
import qualified Storage.Beam.FareBreakupInfo as Beam
import Storage.Queries.FareBreakupInfoExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareBreakupInfo.FareBreakupInfo -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FareBreakupInfo.FareBreakupInfo] -> m ())
createMany = traverse_ create

findByEntityIdAndEntityType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Domain.Types.FareBreakup.FareBreakupEntityType -> m (Maybe Domain.Types.FareBreakupInfo.FareBreakupInfo))
findByEntityIdAndEntityType entityId entityType = do findOneWithKV [Se.And [Se.Is Beam.entityId $ Se.Eq entityId, Se.Is Beam.entityType $ Se.Eq entityType]]

updateFareBreakupsByEntityIdAndEntityType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Domain.Types.FareBreakupInfo.FareBreakupInfoItem] -> Kernel.Prelude.Text -> Domain.Types.FareBreakup.FareBreakupEntityType -> m ())
updateFareBreakupsByEntityIdAndEntityType fareBreakups entityId entityType = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.fareBreakups (Kernel.Utils.Text.encodeToText fareBreakups), Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.entityType $ Se.Eq entityType
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FareBreakupInfo.FareBreakupInfo -> m (Maybe Domain.Types.FareBreakupInfo.FareBreakupInfo))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareBreakupInfo.FareBreakupInfo -> m ())
updateByPrimaryKey (Domain.Types.FareBreakupInfo.FareBreakupInfo {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.entityId entityId,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.fareBreakups (Kernel.Utils.Text.encodeToText fareBreakups),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
