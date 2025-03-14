{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VoipCallStatusExtra where

import Domain.Types.VoipCallStatus
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VoipCallStatus as BeamVCS
import Storage.Queries.OrphanInstances.VoipCallStatus

-- Upsert function for VoipCallStatus
upsert :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => VoipCallStatus -> m ()
upsert voipCallStatus = do
  let callId = voipCallStatus.callId
  if callId /= Just ""
    then do
      existingRecord <- findOneWithKV [Se.Is BeamVCS.callId $ Se.Eq callId]
      if isJust existingRecord
        then do
          updateOneWithKV
            [ Se.Set BeamVCS.callStatus (voipCallStatus.callStatus),
              Se.Set BeamVCS.errorCode (voipCallStatus.errorCode),
              Se.Set BeamVCS.networkType (voipCallStatus.networkType),
              Se.Set BeamVCS.networkQuality (voipCallStatus.networkQuality),
              Se.Set BeamVCS.updatedAt (voipCallStatus.updatedAt)
            ]
            [Se.Is BeamVCS.callId (Se.Eq callId)]
        else do
          createWithKV voipCallStatus
    else do
      createWithKV voipCallStatus
