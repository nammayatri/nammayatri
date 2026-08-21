{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.BecknConfig where

import qualified Domain.Types
import qualified Domain.Types.BecknConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.BecknConfig as Beam

instance FromTType' Beam.BecknConfig Domain.Types.BecknConfig.BecknConfig where
  fromTType' (Beam.BecknConfigT {..}) = do
    gatewayUrl' <- Kernel.Prelude.parseBaseUrl gatewayUrl
    registryUrl' <- Kernel.Prelude.parseBaseUrl registryUrl
    staticTermsUrl' <- ((Kernel.Prelude.maybe (return Kernel.Prelude.Nothing) (Kernel.Prelude.fmap Kernel.Prelude.Just . parseBaseUrl))) staticTermsUrl
    subscriberUrl' <- Kernel.Prelude.parseBaseUrl subscriberUrl
    pure $
      Just
        Domain.Types.BecknConfig.BecknConfig
          { becknProtocol = becknProtocol,
            buyerFinderFee = buyerFinderFee,
            collectedBy = collectedBy,
            domain = domain,
            gatewayUrl = gatewayUrl',
            id = Kernel.Types.Id.Id id,
            multimodalOnSearchTTLSec = multimodalOnSearchTTLSec,
            networkId = networkId,
            onCancelTTLSec = onCancelTTLSec,
            onConfirmTTLSec = onConfirmTTLSec,
            onInitTTLSec = onInitTTLSec,
            onSearchTTLSec = onSearchTTLSec,
            onSelectTTLSec = onSelectTTLSec,
            onStatusTTLSec = onStatusTTLSec,
            onTrackTTLSec = onTrackTTLSec,
            onUpdateTTLSec = onUpdateTTLSec,
            paymentParamsJson = paymentParamsJson,
            registryUrl = registryUrl',
            sendOndcCancellationCodes = sendOndcCancellationCodes,
            settlementType = settlementType,
            settlementWindow = settlementWindow,
            staticTermsUrl = staticTermsUrl',
            subscriberId = subscriberId,
            subscriberUrl = subscriberUrl',
            uniqueKeyId = uniqueKeyId,
            vehicleCategory = vehicleCategory,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.BecknConfig Domain.Types.BecknConfig.BecknConfig where
  toTType' (Domain.Types.BecknConfig.BecknConfig {..}) = do
    Beam.BecknConfigT
      { Beam.becknProtocol = becknProtocol,
        Beam.buyerFinderFee = buyerFinderFee,
        Beam.collectedBy = collectedBy,
        Beam.domain = domain,
        Beam.gatewayUrl = Kernel.Prelude.showBaseUrl gatewayUrl,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.multimodalOnSearchTTLSec = multimodalOnSearchTTLSec,
        Beam.networkId = networkId,
        Beam.onCancelTTLSec = onCancelTTLSec,
        Beam.onConfirmTTLSec = onConfirmTTLSec,
        Beam.onInitTTLSec = onInitTTLSec,
        Beam.onSearchTTLSec = onSearchTTLSec,
        Beam.onSelectTTLSec = onSelectTTLSec,
        Beam.onStatusTTLSec = onStatusTTLSec,
        Beam.onTrackTTLSec = onTrackTTLSec,
        Beam.onUpdateTTLSec = onUpdateTTLSec,
        Beam.paymentParamsJson = paymentParamsJson,
        Beam.registryUrl = Kernel.Prelude.showBaseUrl registryUrl,
        Beam.sendOndcCancellationCodes = sendOndcCancellationCodes,
        Beam.settlementType = settlementType,
        Beam.settlementWindow = settlementWindow,
        Beam.staticTermsUrl = (Kernel.Prelude.fmap showBaseUrl) staticTermsUrl,
        Beam.subscriberId = subscriberId,
        Beam.subscriberUrl = Kernel.Prelude.showBaseUrl subscriberUrl,
        Beam.uniqueKeyId = uniqueKeyId,
        Beam.vehicleCategory = vehicleCategory,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
