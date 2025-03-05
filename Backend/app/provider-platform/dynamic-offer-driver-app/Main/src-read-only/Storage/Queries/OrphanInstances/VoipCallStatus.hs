{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.VoipCallStatus where

import qualified Domain.Types.VoipCallStatus
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.VoipCallStatus as Beam

instance FromTType' Beam.VoipCallStatus Domain.Types.VoipCallStatus.VoipCallStatus where
  fromTType' (Beam.VoipCallStatusT {..}) = do
    pure $
      Just
        Domain.Types.VoipCallStatus.VoipCallStatus
          { callId = callId,
            callStatus = callStatus,
            createdAt = createdAt,
            errorCode = errorCode,
            id = Kernel.Types.Id.Id id,
            merchantCity = merchantCity,
            merchantId = Kernel.Types.Id.Id merchantId,
            networkQuality = networkQuality,
            networkType = networkType,
            rideId = Kernel.Types.Id.Id rideId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VoipCallStatus Domain.Types.VoipCallStatus.VoipCallStatus where
  toTType' (Domain.Types.VoipCallStatus.VoipCallStatus {..}) = do
    Beam.VoipCallStatusT
      { Beam.callId = callId,
        Beam.callStatus = callStatus,
        Beam.createdAt = createdAt,
        Beam.errorCode = errorCode,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantCity = merchantCity,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.networkQuality = networkQuality,
        Beam.networkType = networkType,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.updatedAt = updatedAt
      }
