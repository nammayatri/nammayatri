{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.FinanceRefTypeConfig where

import qualified Data.Aeson
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Lib.Finance.Domain.Types.FinanceRefTypeConfig
import qualified Lib.Finance.Storage.Beam.FinanceRefTypeConfig as Beam
import qualified Lib.Finance.Types.ChargeValue

instance FromTType' Beam.FinanceRefTypeConfig Lib.Finance.Domain.Types.FinanceRefTypeConfig.FinanceRefTypeConfig where
  fromTType' (Beam.FinanceRefTypeConfigT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.FinanceRefTypeConfig.FinanceRefTypeConfig
          { commissionValue = Lib.Finance.Types.ChargeValue.mkChargeValue commissionValueType commissionValueAmount,
            createdAt = createdAt,
            directTaxRates = directTaxRates >>= Kernel.Utils.JSON.valueToMaybe,
            enabled = enabled,
            id = Kernel.Types.Id.Id id,
            indirectTaxDirection = indirectTaxDirection,
            isTaxExclusive = isTaxExclusive,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            referenceType = referenceType,
            taxRate = Lib.Finance.Types.ChargeValue.mkChargeValue taxRateType taxRateValue,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FinanceRefTypeConfig Lib.Finance.Domain.Types.FinanceRefTypeConfig.FinanceRefTypeConfig where
  toTType' (Lib.Finance.Domain.Types.FinanceRefTypeConfig.FinanceRefTypeConfig {..}) = do
    Beam.FinanceRefTypeConfigT
      { Beam.commissionValueAmount = Lib.Finance.Types.ChargeValue.chargeValueAmount <$> commissionValue,
        Beam.commissionValueType = Lib.Finance.Types.ChargeValue.chargeValueType <$> commissionValue,
        Beam.createdAt = createdAt,
        Beam.directTaxRates = Data.Aeson.toJSON <$> directTaxRates,
        Beam.enabled = enabled,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.indirectTaxDirection = indirectTaxDirection,
        Beam.isTaxExclusive = isTaxExclusive,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.referenceType = referenceType,
        Beam.taxRateType = Lib.Finance.Types.ChargeValue.chargeValueType <$> taxRate,
        Beam.taxRateValue = Lib.Finance.Types.ChargeValue.chargeValueAmount <$> taxRate,
        Beam.updatedAt = updatedAt
      }
