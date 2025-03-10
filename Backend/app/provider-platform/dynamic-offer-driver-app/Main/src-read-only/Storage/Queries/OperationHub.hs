{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OperationHub where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.OperationHub
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.OperationHub as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OperationHub.OperationHub -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.OperationHub.OperationHub] -> m ())
createMany = traverse_ create

findAllByCityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.OperationHub.OperationHub]))
findAllByCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.OperationHub.OperationHub -> m (Maybe Domain.Types.OperationHub.OperationHub))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.OperationHub.OperationHub -> m ())
updateByPrimaryKey (Domain.Types.OperationHub.OperationHub {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.mobileNumber mobileNumber,
      Se.Set Beam.name name,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.OperationHub Domain.Types.OperationHub.OperationHub where
  fromTType' (Beam.OperationHubT {..}) = do
    pure $
      Just
        Domain.Types.OperationHub.OperationHub
          { address = address,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            mobileNumber = mobileNumber,
            name = name,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.OperationHub Domain.Types.OperationHub.OperationHub where
  toTType' (Domain.Types.OperationHub.OperationHub {..}) = do
    Beam.OperationHubT
      { Beam.address = address,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.mobileNumber = mobileNumber,
        Beam.name = name,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
