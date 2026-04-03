{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.MerchantPaymentMethod where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.Transformers.MerchantPaymentMethod
import qualified Domain.Types.MerchantPaymentMethod
import qualified Storage.Beam.MerchantPaymentMethod as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.MerchantPaymentMethod Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod
    where fromTType' (Beam.MerchantPaymentMethodT {..}) = do pure $ Just Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod{collectedBy = collectedBy,
                                                                                                                                  createdAt = createdAt,
                                                                                                                                  id = Kernel.Types.Id.Id id,
                                                                                                                                  merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                                  merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                                  paymentInstrument = paymentInstrument,
                                                                                                                                  paymentType = paymentTypeTrans paymentType,
                                                                                                                                  priority = priority,
                                                                                                                                  updatedAt = updatedAt}
instance ToTType' Beam.MerchantPaymentMethod Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod
    where toTType' (Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod {..}) = do Beam.MerchantPaymentMethodT{Beam.collectedBy = collectedBy,
                                                                                                                    Beam.createdAt = createdAt,
                                                                                                                    Beam.id = Kernel.Types.Id.getId id,
                                                                                                                    Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                                    Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                                    Beam.paymentInstrument = paymentInstrument,
                                                                                                                    Beam.paymentType = paymentType,
                                                                                                                    Beam.priority = priority,
                                                                                                                    Beam.updatedAt = updatedAt}



