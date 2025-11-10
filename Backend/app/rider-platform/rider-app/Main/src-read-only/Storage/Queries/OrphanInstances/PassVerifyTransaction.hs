{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PassVerifyTransaction where

import qualified Domain.Types.PassVerifyTransaction
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PassVerifyTransaction as Beam

instance FromTType' Beam.PassVerifyTransaction Domain.Types.PassVerifyTransaction.PassVerifyTransaction where
  fromTType' (Beam.PassVerifyTransactionT {..}) = do
    pure $
      Just
        Domain.Types.PassVerifyTransaction.PassVerifyTransaction
          { destinationStopCode = destinationStopCode,
            fleetId = fleetId,
            id = Kernel.Types.Id.Id id,
            purchasePassId = Kernel.Types.Id.Id purchasePassId,
            sourceStopCode = sourceStopCode,
            validTill = validTill,
            verifiedAt = verifiedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PassVerifyTransaction Domain.Types.PassVerifyTransaction.PassVerifyTransaction where
  toTType' (Domain.Types.PassVerifyTransaction.PassVerifyTransaction {..}) = do
    Beam.PassVerifyTransactionT
      { Beam.destinationStopCode = destinationStopCode,
        Beam.fleetId = fleetId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.purchasePassId = Kernel.Types.Id.getId purchasePassId,
        Beam.sourceStopCode = sourceStopCode,
        Beam.validTill = validTill,
        Beam.verifiedAt = verifiedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
