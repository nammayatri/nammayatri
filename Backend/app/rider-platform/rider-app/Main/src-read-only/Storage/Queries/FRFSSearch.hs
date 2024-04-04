{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSSearch where

import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSSearch as Beam

create :: KvDbFlow m r => (Domain.Types.FRFSSearch.FRFSSearch -> m ())
create = createWithKV

createMany :: KvDbFlow m r => ([Domain.Types.FRFSSearch.FRFSSearch] -> m ())
createMany = traverse_ create

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m (Maybe Domain.Types.FRFSSearch.FRFSSearch))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]

getTicketPlaces :: KvDbFlow m r => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> m [Domain.Types.FRFSSearch.FRFSSearch])
getTicketPlaces merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)]

findByPrimaryKey :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m (Maybe Domain.Types.FRFSSearch.FRFSSearch))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq id]]

updateByPrimaryKey :: KvDbFlow m r => (Domain.Types.FRFSSearch.FRFSSearch -> m ())
updateByPrimaryKey (Domain.Types.FRFSSearch.FRFSSearch {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.fromStationId (Kernel.Types.Id.getId fromStationId),
      Se.Set Beam.quantity quantity,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.toStationId (Kernel.Types.Id.getId toStationId),
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSSearch Domain.Types.FRFSSearch.FRFSSearch where
  fromTType' (Beam.FRFSSearchT {..}) = do
    pure $
      Just
        Domain.Types.FRFSSearch.FRFSSearch
          { fromStationId = Kernel.Types.Id.Id fromStationId,
            id = Kernel.Types.Id.Id id,
            quantity = quantity,
            riderId = Kernel.Types.Id.Id riderId,
            toStationId = Kernel.Types.Id.Id toStationId,
            vehicleType = vehicleType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSSearch Domain.Types.FRFSSearch.FRFSSearch where
  toTType' (Domain.Types.FRFSSearch.FRFSSearch {..}) = do
    Beam.FRFSSearchT
      { Beam.fromStationId = Kernel.Types.Id.getId fromStationId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.quantity = quantity,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.toStationId = Kernel.Types.Id.getId toStationId,
        Beam.vehicleType = vehicleType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
