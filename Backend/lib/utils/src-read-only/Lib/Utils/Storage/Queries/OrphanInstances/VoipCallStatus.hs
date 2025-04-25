{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Utils.Storage.Queries.OrphanInstances.VoipCallStatus where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Utils.Storage.Beam.VoipCallStatus as Beam
import qualified Lib.Utils.Types.VoipCallStatus

instance FromTType' Beam.VoipCallStatus Lib.Utils.Types.VoipCallStatus.VoipCallStatus where
  fromTType' (Beam.VoipCallStatusT {..}) = do
    pure $
      Just
        Lib.Utils.Types.VoipCallStatus.VoipCallStatus
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

instance ToTType' Beam.VoipCallStatus Lib.Utils.Types.VoipCallStatus.VoipCallStatus where
  toTType' (Lib.Utils.Types.VoipCallStatus.VoipCallStatus {..}) = do
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
