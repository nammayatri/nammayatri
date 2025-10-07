{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PayoutInstanceCalculation where

import qualified Domain.Types.PayoutInstanceCalculation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PayoutInstanceCalculation as Beam

instance FromTType' Beam.PayoutInstanceCalculation Domain.Types.PayoutInstanceCalculation.PayoutInstanceCalculation where
  fromTType' (Beam.PayoutInstanceCalculationT {..}) = do
    pure $
      Just
        Domain.Types.PayoutInstanceCalculation.PayoutInstanceCalculation
          { endTime = endTime,
            fromVendorId = fromVendorId,
            id = Kernel.Types.Id.Id id,
            instanceBalance = instanceBalance,
            runningBalance = runningBalance,
            settlementDate = settlementDate,
            settlementMode = settlementMode,
            startTime = startTime,
            status = status,
            toVendorId = toVendorId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutInstanceCalculation Domain.Types.PayoutInstanceCalculation.PayoutInstanceCalculation where
  toTType' (Domain.Types.PayoutInstanceCalculation.PayoutInstanceCalculation {..}) = do
    Beam.PayoutInstanceCalculationT
      { Beam.endTime = endTime,
        Beam.fromVendorId = fromVendorId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.instanceBalance = instanceBalance,
        Beam.runningBalance = runningBalance,
        Beam.settlementDate = settlementDate,
        Beam.settlementMode = settlementMode,
        Beam.startTime = startTime,
        Beam.status = status,
        Beam.toVendorId = toVendorId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
