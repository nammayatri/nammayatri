{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverFyEarnings where

import qualified Domain.Types.DriverFyEarnings
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverFyEarnings as Beam

instance FromTType' Beam.DriverFyEarnings Domain.Types.DriverFyEarnings.DriverFyEarnings where
  fromTType' (Beam.DriverFyEarningsT {..}) = do
    pure $
      Just
        Domain.Types.DriverFyEarnings.DriverFyEarnings
          { financialYear = financialYear,
            id = Kernel.Types.Id.Id id,
            netEarningsTotal = netEarningsTotal,
            personId = Kernel.Types.Id.Id personId,
            quarter = quarter,
            tdsAmountTotal = tdsAmountTotal,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverFyEarnings Domain.Types.DriverFyEarnings.DriverFyEarnings where
  toTType' (Domain.Types.DriverFyEarnings.DriverFyEarnings {..}) = do
    Beam.DriverFyEarningsT
      { Beam.financialYear = financialYear,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.netEarningsTotal = netEarningsTotal,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.quarter = quarter,
        Beam.tdsAmountTotal = tdsAmountTotal,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
