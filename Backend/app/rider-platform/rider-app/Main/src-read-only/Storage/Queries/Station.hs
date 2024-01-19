{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Station where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
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

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.Station.Station -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.Station.Station] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Station.Station -> m (Maybe (Domain.Types.Station.Station))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByStationCode :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> m (Maybe (Domain.Types.Station.Station))
findByStationCode code = do
  findOneWithKV
    [ Se.Is Beam.code $ Se.Eq code
    ]

getTicketPlacesByMerchantOperatingCityIdAndVehicleType :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Domain.Types.Station.FRFSVehicleType -> m ([Domain.Types.Station.Station])
getTicketPlacesByMerchantOperatingCityIdAndVehicleType merchantOperatingCityId vehicleType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType
        ]
    ]

getTicketPlacesByVehicleType :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.Station.FRFSVehicleType -> m ([Domain.Types.Station.Station])
getTicketPlacesByVehicleType vehicleType = do
  findAllWithKV
    [ Se.Is Beam.vehicleType $ Se.Eq vehicleType
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Station.Station -> m (Maybe (Domain.Types.Station.Station))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.Station.Station -> m ()
updateByPrimaryKey Domain.Types.Station.Station {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address $ address,
      Se.Set Beam.code $ code,
      Se.Set Beam.lat $ lat,
      Se.Set Beam.lon $ lon,
      Se.Set Beam.name $ name,
      Se.Set Beam.vehicleType $ vehicleType,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.Station Domain.Types.Station.Station where
  fromTType' Beam.StationT {..} = do
    pure $
      Just
        Domain.Types.Station.Station
          { address = address,
            code = code,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            name = name,
            vehicleType = vehicleType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Station Domain.Types.Station.Station where
  toTType' Domain.Types.Station.Station {..} = do
    Beam.StationT
      { Beam.address = address,
        Beam.code = code,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.name = name,
        Beam.vehicleType = vehicleType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
