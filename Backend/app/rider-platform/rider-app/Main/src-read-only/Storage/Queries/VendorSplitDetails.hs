{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VendorSplitDetails where

import qualified Data.Text
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VendorSplitDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VendorSplitDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VendorSplitDetails.VendorSplitDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VendorSplitDetails.VendorSplitDetails] -> m ())
createMany = traverse_ create

findAllByIntegratedBPPConfigId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m [Domain.Types.VendorSplitDetails.VendorSplitDetails])
findAllByIntegratedBPPConfigId integratedBPPConfigId = do findAllWithKV [Se.Is Beam.integratedBPPConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBPPConfigId)]

findAllByMerchantOperatingCityIdAndIncludeInSplit ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> m [Domain.Types.VendorSplitDetails.VendorSplitDetails])
findAllByMerchantOperatingCityIdAndIncludeInSplit merchantOperatingCityId includeInSplit = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.includeInSplit $ Se.Eq includeInSplit
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.VendorSplitDetails.VendorSplitDetails -> m (Maybe Domain.Types.VendorSplitDetails.VendorSplitDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VendorSplitDetails.VendorSplitDetails -> m ())
updateByPrimaryKey (Domain.Types.VendorSplitDetails.VendorSplitDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.includeInSplit includeInSplit,
      Se.Set Beam.integratedBPPConfigId (Kernel.Types.Id.getId integratedBPPConfigId),
      Se.Set Beam.splitShare (Just $ show splitShare),
      Se.Set Beam.splitType splitType,
      Se.Set Beam.vendorId vendorId,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VendorSplitDetails Domain.Types.VendorSplitDetails.VendorSplitDetails where
  fromTType' (Beam.VendorSplitDetailsT {..}) = do
    pure $
      Just
        Domain.Types.VendorSplitDetails.VendorSplitDetails
          { id = Kernel.Types.Id.Id id,
            includeInSplit = includeInSplit,
            integratedBPPConfigId = Kernel.Types.Id.Id integratedBPPConfigId,
            splitShare = (readMaybe . Data.Text.unpack) =<< splitShare,
            splitType = splitType,
            vendorId = vendorId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VendorSplitDetails Domain.Types.VendorSplitDetails.VendorSplitDetails where
  toTType' (Domain.Types.VendorSplitDetails.VendorSplitDetails {..}) = do
    Beam.VendorSplitDetailsT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.includeInSplit = includeInSplit,
        Beam.integratedBPPConfigId = Kernel.Types.Id.getId integratedBPPConfigId,
        Beam.splitShare = Just $ show splitShare,
        Beam.splitType = splitType,
        Beam.vendorId = vendorId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
