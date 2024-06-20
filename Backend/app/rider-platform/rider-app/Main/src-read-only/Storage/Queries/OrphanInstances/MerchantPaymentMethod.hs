{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MerchantPaymentMethod where

import qualified Domain.Types.MerchantPaymentMethod
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.MerchantPaymentMethod as Beam
import Storage.Queries.Transformers.MerchantPaymentMethod

instance FromTType' Beam.MerchantPaymentMethod Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod where
  fromTType' (Beam.MerchantPaymentMethodT {..}) = do
    pure $
      Just
        Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod
          { id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            paymentType = paymentTypeTrans paymentType,
            paymentInstrument = paymentInstrument,
            collectedBy = collectedBy,
            priority = priority,
            updatedAt = updatedAt,
            createdAt = createdAt
          }

instance ToTType' Beam.MerchantPaymentMethod Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod where
  toTType' (Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod {..}) = do
    Beam.MerchantPaymentMethodT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.paymentType = paymentType,
        Beam.paymentInstrument = paymentInstrument,
        Beam.collectedBy = collectedBy,
        Beam.priority = priority,
        Beam.updatedAt = updatedAt,
        Beam.createdAt = createdAt
      }
