{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.CallStatus where

import qualified Domain.Types.CallStatus
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.CallStatus as Beam

instance FromTType' Beam.CallStatus Domain.Types.CallStatus.CallStatus where
  fromTType' (Beam.CallStatusT {..}) = do
    pure $
      Just
        Domain.Types.CallStatus.CallStatus
          { callError = callError,
            callId = callId,
            callService = callService,
            conversationDuration = conversationDuration,
            createdAt = createdAt,
            dtmfNumberUsed = dtmfNumberUsed,
            entityId = entityId,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            recordingUrl = recordingUrl,
            status = status
          }

instance ToTType' Beam.CallStatus Domain.Types.CallStatus.CallStatus where
  toTType' (Domain.Types.CallStatus.CallStatus {..}) = do
    Beam.CallStatusT
      { Beam.callError = callError,
        Beam.callId = callId,
        Beam.callService = callService,
        Beam.conversationDuration = conversationDuration,
        Beam.createdAt = createdAt,
        Beam.dtmfNumberUsed = dtmfNumberUsed,
        Beam.entityId = entityId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.recordingUrl = recordingUrl,
        Beam.status = status
      }
