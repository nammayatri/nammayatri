{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketCategoryMetadataConfig where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.FRFSQuoteCategoryType
import qualified Domain.Types.FRFSTicketCategoryMetadataConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketCategoryMetadataConfig as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig] -> m ())
createMany = traverse_ create

findByCategoryVehicleAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType -> BecknV2.FRFS.Enums.VehicleCategory -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig))
findByCategoryVehicleAndCity category vehicleCategory merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.category $ Se.Eq category,
          Se.Is Beam.vehicleCategory $ Se.Eq vehicleCategory,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig -> m (Maybe Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig -> m (Maybe Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig -> m ())
updateByPrimaryKey (Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.category category,
      Se.Set Beam.code code,
      Se.Set Beam.description description,
      Se.Set Beam.domainCategoryValue domainCategoryValue,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.title title,
      Se.Set Beam.tnc tnc,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSTicketCategoryMetadataConfig Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig where
  fromTType' (Beam.FRFSTicketCategoryMetadataConfigT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig
          { category = category,
            code = code,
            description = description,
            domainCategoryValue = domainCategoryValue,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            title = title,
            tnc = tnc,
            vehicleCategory = vehicleCategory,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicketCategoryMetadataConfig Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig where
  toTType' (Domain.Types.FRFSTicketCategoryMetadataConfig.FRFSTicketCategoryMetadataConfig {..}) = do
    Beam.FRFSTicketCategoryMetadataConfigT
      { Beam.category = category,
        Beam.code = code,
        Beam.description = description,
        Beam.domainCategoryValue = domainCategoryValue,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.title = title,
        Beam.tnc = tnc,
        Beam.vehicleCategory = vehicleCategory,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
