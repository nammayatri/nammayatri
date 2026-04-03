{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.CallStatus where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.CallStatus
import qualified Storage.Beam.CallStatus as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.CallStatus Domain.Types.CallStatus.CallStatus
    where fromTType' (Beam.CallStatusT {..}) = do pure $ Just Domain.Types.CallStatus.CallStatus{callAttempt = callAttempt,
                                                                                                 callError = callError,
                                                                                                 callId = callId,
                                                                                                 callService = callService,
                                                                                                 conversationDuration = conversationDuration,
                                                                                                 createdAt = createdAt,
                                                                                                 customerIvrResponse = customerIvrResponse,
                                                                                                 dtmfNumberUsed = dtmfNumberUsed,
                                                                                                 id = Kernel.Types.Id.Id id,
                                                                                                 merchantId = merchantId,
                                                                                                 merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                                 recordingUrl = recordingUrl,
                                                                                                 rideId = Kernel.Types.Id.Id <$> rideId,
                                                                                                 status = status,
                                                                                                 updatedAt = updatedAt}
instance ToTType' Beam.CallStatus Domain.Types.CallStatus.CallStatus
    where toTType' (Domain.Types.CallStatus.CallStatus {..}) = do Beam.CallStatusT{Beam.callAttempt = callAttempt,
                                                                                   Beam.callError = callError,
                                                                                   Beam.callId = callId,
                                                                                   Beam.callService = callService,
                                                                                   Beam.conversationDuration = conversationDuration,
                                                                                   Beam.createdAt = createdAt,
                                                                                   Beam.customerIvrResponse = customerIvrResponse,
                                                                                   Beam.dtmfNumberUsed = dtmfNumberUsed,
                                                                                   Beam.id = Kernel.Types.Id.getId id,
                                                                                   Beam.merchantId = merchantId,
                                                                                   Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                                   Beam.recordingUrl = recordingUrl,
                                                                                   Beam.rideId = Kernel.Types.Id.getId <$> rideId,
                                                                                   Beam.status = status,
                                                                                   Beam.updatedAt = updatedAt}



