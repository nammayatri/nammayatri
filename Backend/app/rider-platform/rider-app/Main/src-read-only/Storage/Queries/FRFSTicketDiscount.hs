{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketDiscount where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.FRFSTicketDiscount
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketDiscount as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount] -> m ())
createMany = traverse_ create

findByCodeAndVehicleAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> BecknV2.FRFS.Enums.VehicleCategory -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount))
findByCodeAndVehicleAndCity code vehicleType merchantId merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.code $ Se.Eq code,
          Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByIdAndVehicleAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount -> BecknV2.FRFS.Enums.VehicleCategory -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount))
findByIdAndVehicleAndCity id vehicleType merchantId merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id),
          Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount -> m (Maybe Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount -> m ())
updateByPrimaryKey (Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.code code,
      Se.Set Beam.currency currency,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.ticketCategoryMetadataConfigId (Kernel.Types.Id.getId ticketCategoryMetadataConfigId),
      Se.Set Beam.value value,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSTicketDiscount Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount where
  fromTType' (Beam.FRFSTicketDiscountT {..}) = do
    pure $
      Just
        Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount
          { code = code,
            currency = currency,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            ticketCategoryMetadataConfigId = Kernel.Types.Id.Id ticketCategoryMetadataConfigId,
            value = value,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicketDiscount Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount where
  toTType' (Domain.Types.FRFSTicketDiscount.FRFSTicketDiscount {..}) = do
    Beam.FRFSTicketDiscountT
      { Beam.code = code,
        Beam.currency = currency,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.ticketCategoryMetadataConfigId = Kernel.Types.Id.getId ticketCategoryMetadataConfigId,
        Beam.value = value,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
