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

import Domain.Types.CallStatus
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.CallStatus as BeamCS
-- import Storage.Tabular.CallStatus ()
import qualified Tools.Call as Call

-- import qualified EulerHS.KVConnector.Flow as KV
-- import qualified EulerHS.Language as L
-- import qualified Kernel.Beam.Types as KBT
-- import qualified Sequelize as Se
-- import EulerHS.KVConnector.Types
-- import Lib.Utils (setMeshConfig)

-- Need to update this according to createUnique
-- create :: L.MonadFlow m => CallStatus -> m (MeshResult ())
-- create callStatus = do
--   dbConf <- L.getOption KBT.PsqlDbCfg
--   let modelName = Se.modelTableName @BeamCS.CallStatusT
--   updatedMeshConfig <- setMeshConfig modelName
--   case dbConf of
--     Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainRideToBeam CallStatus)
--     Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- create :: CallStatus -> SqlDB ()
-- create callStatus = void $ Esq.createUnique callStatus

create :: (L.MonadFlow m, Log m) => CallStatus -> m ()
create = createWithKV

-- findById :: Transactionable m => Id CallStatus -> m (Maybe CallStatus)
-- findById = Esq.findById

findById :: (L.MonadFlow m, Log m) => Id CallStatus -> m (Maybe CallStatus)
findById (Id callStatusId) = findOneWithKV [Se.Is BeamCS.id $ Se.Eq callStatusId]

findByIdInReplica :: (L.MonadFlow m, Log m) => Id CallStatus -> m (Maybe CallStatus)
findByIdInReplica (Id callStatusId) = findOneWithKvInReplica [Se.Is BeamCS.id $ Se.Eq callStatusId]

-- findByCallSid :: Transactionable m => Text -> m (Maybe CallStatus)
-- findByCallSid callSid =
--   Esq.findOne $ do
--     callStatus <- from $ table @CallStatusT
--     where_ $ callStatus ^. CallStatusCallId ==. val callSid
--     return callStatus

findByCallSid :: (L.MonadFlow m, Log m) => Text -> m (Maybe CallStatus)
findByCallSid callSid = do
  findOneWithKV [Se.Is BeamCS.callId $ Se.Eq callSid]

-- updateCallStatus :: Id CallStatus -> Call.CallStatus -> Int -> Maybe BaseUrl -> SqlDB ()
-- updateCallStatus callId status conversationDuration mbrecordingUrl = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ CallStatusStatus =. val status,
--         CallStatusConversationDuration =. val conversationDuration,
--         CallStatusRecordingUrl =. val (showBaseUrl <$> mbrecordingUrl)
--       ]
--     where_ $ tbl ^. CallStatusId ==. val (getId callId)

updateCallStatus :: (L.MonadFlow m, Log m) => Id CallStatus -> Call.CallStatus -> Int -> Maybe BaseUrl -> m ()
updateCallStatus (Id callId) status conversationDuration recordingUrl =
  updateWithKV
    [ Se.Set BeamCS.conversationDuration conversationDuration,
      Se.Set BeamCS.recordingUrl $ showBaseUrl <$> recordingUrl,
      Se.Set BeamCS.status status
    ]
    [Se.Is BeamCS.callId (Se.Eq callId)]

instance FromTType' BeamCS.CallStatus CallStatus where
  fromTType' BeamCS.CallStatusT {..} = do
    pure $
      Just
        CallStatus
          { id = Id id,
            callId = callId,
            rideId = Id rideId,
            dtmfNumberUsed = dtmfNumberUsed,
            status = status,
            recordingUrl = recordingUrl,
            conversationDuration = conversationDuration,
            createdAt = createdAt
          }

instance ToTType' BeamCS.CallStatus CallStatus where
  toTType' CallStatus {..} = do
    BeamCS.CallStatusT
      { BeamCS.id = getId id,
        BeamCS.callId = callId,
        BeamCS.rideId = getId rideId,
        BeamCS.dtmfNumberUsed = dtmfNumberUsed,
        BeamCS.status = status,
        BeamCS.recordingUrl = recordingUrl,
        BeamCS.conversationDuration = conversationDuration,
        BeamCS.createdAt = createdAt
      }
