{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CashRidesCommission where

import qualified Domain.Types.CashRidesCommission
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CashRidesCommission as Beam

instance FromTType' Beam.CashRidesCommission Domain.Types.CashRidesCommission.CashRidesCommission where
  fromTType' (Beam.CashRidesCommissionT {..}) = do
    pure $
      Just
        Domain.Types.CashRidesCommission.CashRidesCommission
          { amount = amount,
            currency = currency,
            id = Kernel.Types.Id.Id id,
            lastSettlementTime = lastSettlementTime,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            nextSettlementTime = nextSettlementTime,
            numberOfRides = numberOfRides,
            paymentMode = paymentMode,
            personId = Kernel.Types.Id.Id personId,
            personRole = personRole,
            status = status,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CashRidesCommission Domain.Types.CashRidesCommission.CashRidesCommission where
  toTType' (Domain.Types.CashRidesCommission.CashRidesCommission {..}) = do
    Beam.CashRidesCommissionT
      { Beam.amount = amount,
        Beam.currency = currency,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastSettlementTime = lastSettlementTime,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.nextSettlementTime = nextSettlementTime,
        Beam.numberOfRides = numberOfRides,
        Beam.paymentMode = paymentMode,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.personRole = personRole,
        Beam.status = status,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
