{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CallStatus where

import qualified Domain.Types.CallStatus
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CallStatus as Beam

instance FromTType' Beam.CallStatus Domain.Types.CallStatus.CallStatus where
  fromTType' (Beam.CallStatusT {..}) = do
    pure $
      Just
        Domain.Types.CallStatus.CallStatus
          { id = Kernel.Types.Id.Id id,
            callId = callId,
            rideId = Kernel.Types.Id.Id <$> rideId,
            dtmfNumberUsed = dtmfNumberUsed,
            status = status,
            recordingUrl = recordingUrl,
            conversationDuration = conversationDuration,
            merchantId = merchantId,
            callService = callService,
            callError = callError,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.CallStatus Domain.Types.CallStatus.CallStatus where
  toTType' (Domain.Types.CallStatus.CallStatus {..}) = do
    Beam.CallStatusT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.callId = callId,
        Beam.rideId = Kernel.Types.Id.getId <$> rideId,
        Beam.dtmfNumberUsed = dtmfNumberUsed,
        Beam.status = status,
        Beam.recordingUrl = recordingUrl,
        Beam.conversationDuration = conversationDuration,
        Beam.merchantId = merchantId,
        Beam.callService = callService,
        Beam.callError = callError,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
