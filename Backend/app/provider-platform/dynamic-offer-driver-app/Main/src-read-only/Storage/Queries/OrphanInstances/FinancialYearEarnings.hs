{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FinancialYearEarnings where

import qualified Domain.Types.FinancialYearEarnings
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FinancialYearEarnings as Beam

instance FromTType' Beam.FinancialYearEarnings Domain.Types.FinancialYearEarnings.FinancialYearEarnings where
  fromTType' (Beam.FinancialYearEarningsT {..}) = do
    pure $
      Just
        Domain.Types.FinancialYearEarnings.FinancialYearEarnings
          { collectionAmount = collectionAmount,
            driverId = Kernel.Types.Id.Id driverId,
            earningsAmount = earningsAmount,
            financialYearStart = financialYearStart,
            gstDeduction = gstDeduction,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            rideId = Kernel.Types.Id.Id rideId,
            totalEarnings = totalEarnings,
            createdAt = createdAt
          }

instance ToTType' Beam.FinancialYearEarnings Domain.Types.FinancialYearEarnings.FinancialYearEarnings where
  toTType' (Domain.Types.FinancialYearEarnings.FinancialYearEarnings {..}) = do
    Beam.FinancialYearEarningsT
      { Beam.collectionAmount = collectionAmount,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.earningsAmount = earningsAmount,
        Beam.financialYearStart = financialYearStart,
        Beam.gstDeduction = gstDeduction,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.totalEarnings = totalEarnings,
        Beam.createdAt = createdAt
      }
