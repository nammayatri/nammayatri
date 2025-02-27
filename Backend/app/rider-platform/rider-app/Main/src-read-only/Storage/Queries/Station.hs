{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Station (module Storage.Queries.Station, module ReExport) where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Station
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Station as Beam
import Storage.Queries.StationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Station.Station -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Station.Station] -> m ())
createMany = traverse_ create

deleteByStationCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m ())
deleteByStationCode code integratedBppConfigId = do deleteWithKV [Se.And [Se.Is Beam.code $ Se.Eq code, Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)]]

findAllByVehicleType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> BecknV2.FRFS.Enums.VehicleCategory -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m [Domain.Types.Station.Station])
findAllByVehicleType limit offset vehicleType integratedBppConfigId = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Station.Station -> m (Maybe Domain.Types.Station.Station))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByStationCode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m (Maybe Domain.Types.Station.Station))
findByStationCode code integratedBppConfigId = do findOneWithKV [Se.And [Se.Is Beam.code $ Se.Eq code, Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Station.Station -> m (Maybe Domain.Types.Station.Station))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Station.Station -> m ())
updateByPrimaryKey (Domain.Types.Station.Station {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.code code,
      Se.Set Beam.integratedBppConfigId (Kernel.Types.Id.getId integratedBppConfigId),
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.possibleTypes possibleTypes,
      Se.Set Beam.timeBounds (Kernel.Prelude.Just timeBounds),
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
