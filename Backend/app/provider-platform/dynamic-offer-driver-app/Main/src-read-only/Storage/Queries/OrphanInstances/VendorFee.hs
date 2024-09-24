{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.VendorFee where

import qualified Domain.Types.VendorFee
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.VendorFee as Beam

instance FromTType' Beam.VendorFee Domain.Types.VendorFee.VendorFee where
  fromTType' (Beam.VendorFeeT {..}) = do
    pure $
      Just
        Domain.Types.VendorFee.VendorFee
          { amount = amount,
            driverFeeId = Kernel.Types.Id.Id driverFeeId,
            vendorId = vendorId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VendorFee Domain.Types.VendorFee.VendorFee where
  toTType' (Domain.Types.VendorFee.VendorFee {..}) = do
    Beam.VendorFeeT
      { Beam.amount = amount,
        Beam.driverFeeId = Kernel.Types.Id.getId driverFeeId,
        Beam.vendorId = vendorId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
