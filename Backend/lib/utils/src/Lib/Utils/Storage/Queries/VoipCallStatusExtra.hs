{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Utils.Storage.Queries.VoipCallStatusExtra where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Lib.Utils.Storage.Beam.BeamFlow
import qualified Lib.Utils.Storage.Beam.VoipCallStatus as BeamVCS
import Lib.Utils.Storage.Queries.OrphanInstances.VoipCallStatus
import Lib.Utils.Types.VoipCallStatus
import Sequelize as Se

upsert :: (BeamFlow m r) => VoipCallStatus -> m ()
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
