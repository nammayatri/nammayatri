{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.CallStatus where

import qualified Database.Beam as B
import Domain.Types.CallStatus
import Domain.Types.Ride
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Beam.CallStatus as BeamCT
import qualified Storage.Beam.Common as BeamCommon

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => CallStatus -> m ()
create cs = do
  callS <- findByCallSid (cs.callId)
  case callS of
    Nothing -> createWithKV cs
    Just _ -> pure ()

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id CallStatus -> m (Maybe CallStatus)
findById (Id callStatusId) = findOneWithKV [Se.Is BeamCT.id $ Se.Eq callStatusId]

findByCallSid :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> m (Maybe CallStatus)
findByCallSid callSid = findOneWithKV [Se.Is BeamCT.callId $ Se.Eq callSid]

updateCallStatus :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id CallStatus -> Call.CallStatus -> Int -> Maybe Text -> m ()
updateCallStatus (Id callId) status conversationDuration recordingUrl =
  updateWithKV
    [ Set BeamCT.conversationDuration conversationDuration,
      Set BeamCT.recordingUrl recordingUrl,
      Set BeamCT.status status
    ]
    [Is BeamCT.id (Se.Eq callId)]

countCallsByEntityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Ride -> m Int
countCallsByEntityId entityID = do
  dbConf <- getMasterBeamConfig
  resp <-
    L.runDB dbConf $
      L.findRow $
        B.select $
          B.aggregate_ (\ride -> (B.group_ (BeamCT.entityId ride), B.as_ @Int B.countAll_)) $
            B.filter_' (\(BeamCT.CallStatusT {..}) -> entityId B.==?. B.val_ (getId entityID)) $
              B.all_ (BeamCommon.callStatus BeamCommon.atlasDB)
  pure $ either (const 0) (maybe 0 snd) resp

instance FromTType' BeamCT.CallStatus CallStatus where
  fromTType' BeamCT.CallStatusT {..} = do
    pure $
      Just
        CallStatus
          { id = Id id,
            callId = callId,
            entityId = entityId,
            dtmfNumberUsed = dtmfNumberUsed,
            status = status,
            recordingUrl = recordingUrl,
            conversationDuration = conversationDuration,
            createdAt = createdAt
          }

instance ToTType' BeamCT.CallStatus CallStatus where
  toTType' CallStatus {..} = do
    BeamCT.CallStatusT
      { BeamCT.id = getId id,
        BeamCT.callId = callId,
        BeamCT.entityId = entityId,
        BeamCT.dtmfNumberUsed = dtmfNumberUsed,
        BeamCT.status = status,
        BeamCT.recordingUrl = recordingUrl,
        BeamCT.conversationDuration = conversationDuration,
        BeamCT.createdAt = createdAt
      }
