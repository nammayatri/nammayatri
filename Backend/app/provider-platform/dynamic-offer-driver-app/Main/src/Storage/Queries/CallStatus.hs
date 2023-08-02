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
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findOneWithKV, findOneWithKvInReplica, getMasterBeamConfig, getReplicaBeamConfig, updateWithKV)
import Sequelize as Se
import qualified Storage.Beam.CallStatus as BeamCT
import qualified Storage.Beam.Common as BeamCommon

create :: (L.MonadFlow m, Log m) => CallStatus -> m ()
create = createWithKV

findById :: (L.MonadFlow m, Log m) => Id CallStatus -> m (Maybe CallStatus)
findById (Id callStatusId) = findOneWithKV [Se.Is BeamCT.id $ Se.Eq callStatusId]

findByIdInReplica :: (L.MonadFlow m, Log m) => Id CallStatus -> m (Maybe CallStatus)
findByIdInReplica (Id callStatusId) = findOneWithKvInReplica [Se.Is BeamCT.id $ Se.Eq callStatusId]

findByCallSid :: (L.MonadFlow m, Log m) => Text -> m (Maybe CallStatus)
findByCallSid callSid = findOneWithKV [Se.Is BeamCT.callId $ Se.Eq callSid]

-- updateCallStatus :: Id CallStatus -> Call.CallStatus -> Int -> Maybe Text -> SqlDB ()
-- updateCallStatus callId status conversationDuration mbrecordingUrl = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ CallStatusStatus =. val status,
--         CallStatusConversationDuration =. val conversationDuration,
--         CallStatusRecordingUrl =. val mbrecordingUrl
--       ]
--     where_ $ tbl ^. CallStatusId ==. val (getId callId)

updateCallStatus :: (L.MonadFlow m, Log m) => Id CallStatus -> Call.CallStatus -> Int -> Maybe Text -> m ()
updateCallStatus (Id callId) status conversationDuration recordingUrl =
  updateWithKV
    [ Set BeamCT.conversationDuration conversationDuration,
      Set BeamCT.recordingUrl recordingUrl,
      Set BeamCT.status status
    ]
    [Is BeamCT.callId (Se.Eq callId)]

-- countCallsByEntityId :: Transactionable m => Id Ride -> m Int
-- countCallsByEntityId entityId = (fromMaybe 0 <$>) $
--   Esq.findOne $ do
--     callStatus <- from $ table @CallStatusT
--     where_ $ callStatus ^. CallStatusEntityId ==. val (toKey entityId)
--     groupBy $ callStatus ^. CallStatusEntityId
--     pure $ count @Int $ callStatus ^. CallStatusTId

-- to be discussed if it should be a beam query or not
-- countCallsByEntityId :: L.MonadFlow m => Id Ride -> m Int
-- countCallsByEntityId entityID = do
--   dbConf <- getMasterBeamConfig
--   resp <-
--     L.runDB dbConf $
--       L.findRow $
--         B.select $
--           B.aggregate_ (\ride -> (B.group_ (BeamCT.entityId ride), B.as_ @Int B.countAll_)) $
--             B.filter_' (\(BeamCT.CallStatusT {..}) -> entityId B.==?. B.val_ (getId entityID)) $
--               B.all_ (BeamCommon.callStatus BeamCommon.atlasDB)
--   pure $ either (const 0) (maybe 0 snd) resp

countCallsByEntityIdInReplica :: L.MonadFlow m => Id Ride -> m Int
countCallsByEntityIdInReplica entityID = do
  dbConf <- getReplicaBeamConfig
  resp <-
    L.runDB dbConf $
      L.findRow $
        B.select $
          B.aggregate_ (\ride -> (B.group_ (BeamCT.entityId ride), B.as_ @Int B.countAll_)) $
            B.filter_' (\(BeamCT.CallStatusT {..}) -> entityId B.==?. B.val_ (getId entityID)) $
              B.all_ (BeamCommon.callStatus BeamCommon.atlasDB)
  pure $ either (const 0) (maybe 0 snd) resp

countCallsByEntityId :: (L.MonadFlow m) => Id Ride -> m Int
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

-- countCallsByEntityId :: Transactionable m => Id Ride -> m Int
-- countCallsByEntityId entityId = (fromMaybe 0 <$>) $
--   Esq.findOne $ do
--     callStatus <- from $ table @CallStatusT
--     where_ $ callStatus ^. CallStatusEntityId ==. val (entityId.getId)
--     groupBy $ callStatus ^. CallStatusEntityId
--     pure $ count @Int $ callStatus ^. CallStatusTId

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
