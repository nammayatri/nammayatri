{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SplitDetails where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Plan
import qualified Domain.Types.SplitDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SplitDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SplitDetails.SplitDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SplitDetails.SplitDetails] -> m ())
createMany = traverse_ create

findSplitDetailsByMerchantOpCityIdAndServiceName ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Domain.Types.Plan.ServiceNames -> m [Domain.Types.SplitDetails.SplitDetails])
findSplitDetailsByMerchantOpCityIdAndServiceName merchantOperatingCityId serviceName = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SplitDetails.SplitDetails -> m (Maybe Domain.Types.SplitDetails.SplitDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SplitDetails.SplitDetails -> m ())
updateByPrimaryKey (Domain.Types.SplitDetails.SplitDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amountPercentage amountPercentage,
      Se.Set Beam.fixedAmount fixedAmount,
      Se.Set Beam.serviceName serviceName,
      Se.Set Beam.vendorId vendorId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SplitDetails Domain.Types.SplitDetails.SplitDetails where
  fromTType' (Beam.SplitDetailsT {..}) = do
    pure $
      Just
        Domain.Types.SplitDetails.SplitDetails
          { amountPercentage = amountPercentage,
            fixedAmount = fixedAmount,
            id = Kernel.Types.Id.Id id,
            serviceName = serviceName,
            vendorId = vendorId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SplitDetails Domain.Types.SplitDetails.SplitDetails where
  toTType' (Domain.Types.SplitDetails.SplitDetails {..}) = do
    Beam.SplitDetailsT
      { Beam.amountPercentage = amountPercentage,
        Beam.fixedAmount = fixedAmount,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.serviceName = serviceName,
        Beam.vendorId = vendorId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
