{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VendorSplitDetails where

import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleVariant
import qualified Domain.Types.VendorSplitDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Types.SpecialLocation
import qualified Sequelize as Se
import qualified Storage.Beam.VendorSplitDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VendorSplitDetails.VendorSplitDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VendorSplitDetails.VendorSplitDetails] -> m ())
createMany = traverse_ create

findAllByAreasCityAndVariant ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Lib.Types.SpecialLocation.Area] -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.VehicleVariant.VehicleVariant -> m [Domain.Types.VendorSplitDetails.VendorSplitDetails])
findAllByAreasCityAndVariant area merchantOperatingCityId vehicleVariant = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.area $ Se.In area,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleVariant
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Lib.Types.SpecialLocation.Area -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.VehicleVariant.VehicleVariant -> Data.Text.Text -> m (Maybe Domain.Types.VendorSplitDetails.VendorSplitDetails))
findByPrimaryKey area merchantOperatingCityId vehicleVariant vendorId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleVariant,
          Se.Is Beam.vendorId $ Se.Eq vendorId
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VendorSplitDetails.VendorSplitDetails -> m ())
updateByPrimaryKey (Domain.Types.VendorSplitDetails.VendorSplitDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.maxVendorFeeAmount maxVendorFeeAmount, Se.Set Beam.splitType splitType, Se.Set Beam.splitValue splitValue, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.area $ Se.Eq area,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleVariant $ Se.Eq vehicleVariant,
          Se.Is Beam.vendorId $ Se.Eq vendorId
        ]
    ]

instance FromTType' Beam.VendorSplitDetails Domain.Types.VendorSplitDetails.VendorSplitDetails where
  fromTType' (Beam.VendorSplitDetailsT {..}) = do
    pure $
      Just
        Domain.Types.VendorSplitDetails.VendorSplitDetails
          { area = area,
            maxVendorFeeAmount = maxVendorFeeAmount,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            splitType = splitType,
            splitValue = splitValue,
            vehicleVariant = vehicleVariant,
            vendorId = vendorId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VendorSplitDetails Domain.Types.VendorSplitDetails.VendorSplitDetails where
  toTType' (Domain.Types.VendorSplitDetails.VendorSplitDetails {..}) = do
    Beam.VendorSplitDetailsT
      { Beam.area = area,
        Beam.maxVendorFeeAmount = maxVendorFeeAmount,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.splitType = splitType,
        Beam.splitValue = splitValue,
        Beam.vehicleVariant = vehicleVariant,
        Beam.vendorId = vendorId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
