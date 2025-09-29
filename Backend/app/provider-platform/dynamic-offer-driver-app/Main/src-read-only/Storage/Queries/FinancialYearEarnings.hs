{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FinancialYearEarnings where

import qualified Domain.Types.FinancialYearEarnings
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FinancialYearEarnings as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FinancialYearEarnings.FinancialYearEarnings -> m ())
create = createWithKV

instance FromTType' Beam.FinancialYearEarnings Domain.Types.FinancialYearEarnings.FinancialYearEarnings where
  fromTType' (Beam.FinancialYearEarningsT {..}) = do
    pure $
      Just
        Domain.Types.FinancialYearEarnings.FinancialYearEarnings
          { financialYearCollectionAmount = financialYearCollectionAmount,
            financialYearStart = financialYearStart,
            financialYearTdsBaseAmount = financialYearTdsBaseAmount,
            financialYearTdsDeduction = financialYearTdsDeduction,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            createdAt = createdAt
          }

instance ToTType' Beam.FinancialYearEarnings Domain.Types.FinancialYearEarnings.FinancialYearEarnings where
  toTType' (Domain.Types.FinancialYearEarnings.FinancialYearEarnings {..}) = do
    Beam.FinancialYearEarningsT
      { Beam.financialYearCollectionAmount = financialYearCollectionAmount,
        Beam.financialYearStart = financialYearStart,
        Beam.financialYearTdsBaseAmount = financialYearTdsBaseAmount,
        Beam.financialYearTdsDeduction = financialYearTdsDeduction,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.createdAt = createdAt
      }
