{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Station where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Station
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Station as Beam

create :: KvDbFlow m r => (Domain.Types.Station.Station -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.Station.Station] -> m ())
createMany = traverse_ create

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Station.Station -> m (Maybe Domain.Types.Station.Station))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

findByStationCode :: KvDbFlow m r => (Kernel.Prelude.Text -> m (Maybe Domain.Types.Station.Station))
findByStationCode code = do findOneWithKV [Se.Is Beam.code $ Se.Eq code]

getTicketPlacesByMerchantOperatingCityIdAndVehicleType ::
  KvDbFlow m r =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.Station.FRFSVehicleType -> m [Domain.Types.Station.Station])
getTicketPlacesByMerchantOperatingCityIdAndVehicleType (Kernel.Types.Id.Id merchantOperatingCityId) vehicleType = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.vehicleType $ Se.Eq vehicleType
        ]
    ]

getTicketPlacesByVehicleType :: KvDbFlow m r => (Domain.Types.Station.FRFSVehicleType -> m [Domain.Types.Station.Station])
getTicketPlacesByVehicleType vehicleType = do findAllWithKV [Se.Is Beam.vehicleType $ Se.Eq vehicleType]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.Station.Station -> m (Maybe Domain.Types.Station.Station))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.Station.Station -> m ())
updateByPrimaryKey (Domain.Types.Station.Station {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.code code,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Station Domain.Types.Station.Station where
  fromTType' (Beam.StationT {..}) = do
    pure $
      Just
        Domain.Types.Station.Station
          { address = address,
            code = code,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            vehicleType = vehicleType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Station Domain.Types.Station.Station where
  toTType' (Domain.Types.Station.Station {..}) = do
    Beam.StationT
      { Beam.address = address,
        Beam.code = code,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.vehicleType = vehicleType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
