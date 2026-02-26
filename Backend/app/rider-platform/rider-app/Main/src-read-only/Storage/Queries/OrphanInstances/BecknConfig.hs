{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.BecknConfig where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.BecknConfig
import qualified Storage.Beam.BecknConfig as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id



instance FromTType' Beam.BecknConfig Domain.Types.BecknConfig.BecknConfig
    where fromTType' (Beam.BecknConfigT {..}) = do {gatewayUrl' <- Kernel.Prelude.parseBaseUrl gatewayUrl;
                                                    registryUrl' <- Kernel.Prelude.parseBaseUrl registryUrl;
                                                    staticTermsUrl' <- ((Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl))) staticTermsUrl;
                                                    subscriberUrl' <- Kernel.Prelude.parseBaseUrl subscriberUrl;
                                                    pure $ Just Domain.Types.BecknConfig.BecknConfig{bapIFSC = bapIFSC,
                                                                                                     buyerFinderFee = buyerFinderFee,
                                                                                                     cancelTTLSec = cancelTTLSec,
                                                                                                     collectedBy = collectedBy,
                                                                                                     confirmBufferTTLSec = confirmBufferTTLSec,
                                                                                                     confirmTTLSec = confirmTTLSec,
                                                                                                     domain = domain,
                                                                                                     gatewayUrl = gatewayUrl',
                                                                                                     id = Kernel.Types.Id.Id id,
                                                                                                     initTTLSec = initTTLSec,
                                                                                                     paymentParamsJson = paymentParamsJson,
                                                                                                     ppfEnabled = ppfEnabled,
                                                                                                     ratingTTLSec = ratingTTLSec,
                                                                                                     registryUrl = registryUrl',
                                                                                                     searchTTLSec = searchTTLSec,
                                                                                                     selectTTLSec = selectTTLSec,
                                                                                                     settlementType = settlementType,
                                                                                                     settlementWindow = settlementWindow,
                                                                                                     staticTermsUrl = staticTermsUrl',
                                                                                                     statusTTLSec = statusTTLSec,
                                                                                                     subscriberId = subscriberId,
                                                                                                     subscriberUrl = subscriberUrl',
                                                                                                     trackTTLSec = trackTTLSec,
                                                                                                     uniqueKeyId = uniqueKeyId,
                                                                                                     vehicleCategory = vehicleCategory,
                                                                                                     merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                     merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                                     createdAt = createdAt,
                                                                                                     updatedAt = updatedAt}}
instance ToTType' Beam.BecknConfig Domain.Types.BecknConfig.BecknConfig
    where toTType' (Domain.Types.BecknConfig.BecknConfig {..}) = do Beam.BecknConfigT{Beam.bapIFSC = bapIFSC,
                                                                                      Beam.buyerFinderFee = buyerFinderFee,
                                                                                      Beam.cancelTTLSec = cancelTTLSec,
                                                                                      Beam.collectedBy = collectedBy,
                                                                                      Beam.confirmBufferTTLSec = confirmBufferTTLSec,
                                                                                      Beam.confirmTTLSec = confirmTTLSec,
                                                                                      Beam.domain = domain,
                                                                                      Beam.gatewayUrl = Kernel.Prelude.showBaseUrl gatewayUrl,
                                                                                      Beam.id = Kernel.Types.Id.getId id,
                                                                                      Beam.initTTLSec = initTTLSec,
                                                                                      Beam.paymentParamsJson = paymentParamsJson,
                                                                                      Beam.ppfEnabled = ppfEnabled,
                                                                                      Beam.ratingTTLSec = ratingTTLSec,
                                                                                      Beam.registryUrl = Kernel.Prelude.showBaseUrl registryUrl,
                                                                                      Beam.searchTTLSec = searchTTLSec,
                                                                                      Beam.selectTTLSec = selectTTLSec,
                                                                                      Beam.settlementType = settlementType,
                                                                                      Beam.settlementWindow = settlementWindow,
                                                                                      Beam.staticTermsUrl = (Kernel.Prelude.fmap showBaseUrl) staticTermsUrl,
                                                                                      Beam.statusTTLSec = statusTTLSec,
                                                                                      Beam.subscriberId = subscriberId,
                                                                                      Beam.subscriberUrl = Kernel.Prelude.showBaseUrl subscriberUrl,
                                                                                      Beam.trackTTLSec = trackTTLSec,
                                                                                      Beam.uniqueKeyId = uniqueKeyId,
                                                                                      Beam.vehicleCategory = vehicleCategory,
                                                                                      Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                      Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                                      Beam.createdAt = createdAt,
                                                                                      Beam.updatedAt = updatedAt}



