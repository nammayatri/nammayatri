{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.VendorSplitDetails where

import qualified Domain.Types.VendorSplitDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.VendorSplitDetails as Beam

instance FromTType' Beam.VendorSplitDetails Domain.Types.VendorSplitDetails.VendorSplitDetails where
  fromTType' (Beam.VendorSplitDetailsT {..}) = do
    pure $
      Just
        Domain.Types.VendorSplitDetails.VendorSplitDetails
          { area = area,
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
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.splitType = splitType,
        Beam.splitValue = splitValue,
        Beam.vehicleVariant = vehicleVariant,
        Beam.vendorId = vendorId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
