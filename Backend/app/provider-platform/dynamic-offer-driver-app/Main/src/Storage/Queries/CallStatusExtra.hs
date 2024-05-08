{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CallStatusExtra where

import qualified Database.Beam as B
import Domain.Types.CallStatus
import Domain.Types.Ride
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.External.Call.Types (CallService)
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Sequelize as Se
import qualified Storage.Beam.CallStatus as BeamCT
import qualified Storage.Beam.Common as BeamCommon
import Storage.Queries.OrphanInstances.CallStatus

-- Extra code goes here --

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => CallStatus -> m ()
create cs = do
  callS <- findByCallSid (cs.callId)
  case callS of
    Nothing -> createWithKV cs
    Just _ -> pure ()

findByCallSid :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe CallStatus)
findByCallSid callSid = findOneWithKV [Se.Is BeamCT.callId $ Se.Eq callSid]

countCallsByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Ride -> m Int
countCallsByEntityId entityID = do
  dbConf <- getMasterBeamConfig
  resp <-
    L.runDB dbConf $
      L.findRow $
        B.select $
          B.aggregate_ (\ride -> (B.group_ (BeamCT.entityId ride), B.as_ @Int B.countAll_)) $
            B.filter_' (\(BeamCT.CallStatusT {..}) -> B.fromMaybe_ (B.val_ "") entityId B.==?. B.val_ (getId entityID)) $
              B.all_ (BeamCommon.callStatus BeamCommon.atlasDB)
  pure $ either (const 0) (maybe 0 snd) resp
<<<<<<< HEAD

updateCallStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id CallStatus -> Call.CallStatus -> Int -> Maybe Text -> m ()
updateCallStatus (Id callId) status conversationDuration recordingUrl =
  updateWithKV
    [ Set BeamCT.conversationDuration conversationDuration,
      Set BeamCT.recordingUrl recordingUrl,
      Set BeamCT.status status
    ]
    [Is BeamCT.id (Se.Eq callId)]

updateCallError :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Maybe Text -> Maybe Text -> Maybe CallService -> m ()
updateCallError callSid callError merchantId callService =
  updateWithKV
    [ Set BeamCT.callError callError,
      Set BeamCT.callService callService,
      Set BeamCT.merchantId merchantId
    ]
    [Is BeamCT.callId (Se.Eq callSid)]

updateCallStatusWithRideId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id CallStatus -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe CallService -> m ()
updateCallStatusWithRideId (Id callId) rideId dtmfNumberUsed merchantId callService =
  updateWithKV
    [ Set BeamCT.entityId rideId,
      Set BeamCT.dtmfNumberUsed dtmfNumberUsed,
      Set BeamCT.merchantId merchantId,
      Set BeamCT.callService callService
    ]
    [Is BeamCT.id (Se.Eq callId)]
=======
>>>>>>> 2ac75e3b6b (backend/enh/DSL/move-tables-dsl-driver-offer-person-part: CallStatus)
