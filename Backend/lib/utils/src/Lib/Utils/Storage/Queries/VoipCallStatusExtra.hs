{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Utils.Storage.Queries.VoipCallStatusExtra where

import qualified Data.Text as T
import Kernel.Beam.Functions
import Kernel.Prelude
import Lib.Utils.Storage.Beam.BeamFlow
import qualified Lib.Utils.Storage.Beam.VoipCallStatus as BeamVCS
import Lib.Utils.Storage.Queries.OrphanInstances.VoipCallStatus ()
import Lib.Utils.Types.VoipCallStatus
import Sequelize as Se

upsert :: (BeamFlow m r) => VoipCallStatus -> m ()
upsert voipCallStatus
  | Just cid <- voipCallStatus.callId,
    not (T.null cid) = do
    existingRecord <- findOneWithKV [Se.Is BeamVCS.callId $ Se.Eq (Just cid)]
    case existingRecord of
      Just _ ->
        updateOneWithKV
          [ Se.Set BeamVCS.callStatus (voipCallStatus.callStatus),
            Se.Set BeamVCS.errorCode (voipCallStatus.errorCode),
            Se.Set BeamVCS.networkType (voipCallStatus.networkType),
            Se.Set BeamVCS.networkQuality (voipCallStatus.networkQuality),
            Se.Set BeamVCS.updatedAt (voipCallStatus.updatedAt)
          ]
          [Se.Is BeamVCS.callId (Se.Eq (Just cid))]
      Nothing -> createWithKV voipCallStatus
  | otherwise = createWithKV voipCallStatus
