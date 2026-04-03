{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.Mandate where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.Mandate
import qualified Storage.Beam.Mandate as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id



instance FromTType' Beam.Mandate Domain.Types.Mandate.Mandate
    where fromTType' (Beam.MandateT {..}) = do pure $ Just Domain.Types.Mandate.Mandate{createdAt = createdAt,
                                                                                        currency = fromMaybe Kernel.Types.Common.INR currency,
                                                                                        endDate = endDate,
                                                                                        id = Kernel.Types.Id.Id id,
                                                                                        mandatePaymentFlow = mandatePaymentFlow,
                                                                                        maxAmount = maxAmount,
                                                                                        merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                        payerApp = payerApp,
                                                                                        payerAppName = payerAppName,
                                                                                        payerVpa = payerVpa,
                                                                                        startDate = startDate,
                                                                                        status = status,
                                                                                        updatedAt = updatedAt,
                                                                                        merchantId = Kernel.Types.Id.Id <$> merchantId}
instance ToTType' Beam.Mandate Domain.Types.Mandate.Mandate
    where toTType' (Domain.Types.Mandate.Mandate {..}) = do Beam.MandateT{Beam.createdAt = createdAt,
                                                                          Beam.currency = Kernel.Prelude.Just currency,
                                                                          Beam.endDate = endDate,
                                                                          Beam.id = Kernel.Types.Id.getId id,
                                                                          Beam.mandatePaymentFlow = mandatePaymentFlow,
                                                                          Beam.maxAmount = maxAmount,
                                                                          Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                          Beam.payerApp = payerApp,
                                                                          Beam.payerAppName = payerAppName,
                                                                          Beam.payerVpa = payerVpa,
                                                                          Beam.startDate = startDate,
                                                                          Beam.status = status,
                                                                          Beam.updatedAt = updatedAt,
                                                                          Beam.merchantId = Kernel.Types.Id.getId <$> merchantId}



