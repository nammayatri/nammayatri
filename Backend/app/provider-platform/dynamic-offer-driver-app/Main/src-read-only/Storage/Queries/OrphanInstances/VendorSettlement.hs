{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.VendorSettlement where

import qualified Domain.Types.VendorSettlement
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.VendorSettlement as Beam

instance FromTType' Beam.VendorSettlement Domain.Types.VendorSettlement.VendorSettlement where
  fromTType' (Beam.VendorSettlementT {..}) = do
    pure $
      Just
        Domain.Types.VendorSettlement.VendorSettlement
          { createdAt = createdAt,
            fromVendorId = fromVendorId,
            id = Kernel.Types.Id.Id id,
            runningBalance = runningBalance,
            settlementDate = settlementDate,
            settlementMode = settlementMode,
            status = status,
            toVendorId = toVendorId,
            updatedAt = updatedAt,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.VendorSettlement Domain.Types.VendorSettlement.VendorSettlement where
  toTType' (Domain.Types.VendorSettlement.VendorSettlement {..}) = do
    Beam.VendorSettlementT
      { Beam.createdAt = createdAt,
        Beam.fromVendorId = fromVendorId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.runningBalance = runningBalance,
        Beam.settlementDate = settlementDate,
        Beam.settlementMode = settlementMode,
        Beam.status = status,
        Beam.toVendorId = toVendorId,
        Beam.updatedAt = updatedAt,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
