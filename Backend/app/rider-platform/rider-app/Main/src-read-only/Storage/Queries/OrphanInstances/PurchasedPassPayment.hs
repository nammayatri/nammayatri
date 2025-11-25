{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PurchasedPassPayment where

import qualified Domain.Types.PurchasedPassPayment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PurchasedPassPayment as Beam

instance FromTType' Beam.PurchasedPassPayment Domain.Types.PurchasedPassPayment.PurchasedPassPayment where
  fromTType' (Beam.PurchasedPassPaymentT {..}) = do
    pure $
      Just
        Domain.Types.PurchasedPassPayment.PurchasedPassPayment
          { amount = amount,
            endDate = endDate,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            orderId = Kernel.Types.Id.Id orderId,
            passCode = passCode,
            passName = passName,
            personId = Kernel.Types.Id.Id personId,
            purchasedPassId = Kernel.Types.Id.Id purchasedPassId,
            startDate = startDate,
            status = status,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PurchasedPassPayment Domain.Types.PurchasedPassPayment.PurchasedPassPayment where
  toTType' (Domain.Types.PurchasedPassPayment.PurchasedPassPayment {..}) = do
    Beam.PurchasedPassPaymentT
      { Beam.amount = amount,
        Beam.endDate = endDate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.orderId = Kernel.Types.Id.getId orderId,
        Beam.passCode = passCode,
        Beam.passName = passName,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.purchasedPassId = Kernel.Types.Id.getId purchasedPassId,
        Beam.startDate = startDate,
        Beam.status = status,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
