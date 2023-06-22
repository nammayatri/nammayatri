{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.CallStatus where

import Domain.Types.CallStatus
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.CallStatus as BeamCS
import Storage.Tabular.CallStatus
import qualified Tools.Call as Call

create :: CallStatus -> SqlDB ()
create callStatus = void $ Esq.createUnique callStatus

findById :: Transactionable m => Id CallStatus -> m (Maybe CallStatus)
findById = Esq.findById

findById' :: L.MonadFlow m => Id CallStatus -> m (Maybe CallStatus)
findById' (Id callStatusId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamCS.CallStatusT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamCallStatusToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamCS.id $ Se.Eq callStatusId]
    Nothing -> pure Nothing

findByCallSid :: Transactionable m => Text -> m (Maybe CallStatus)
findByCallSid callSid =
  Esq.findOne $ do
    callStatus <- from $ table @CallStatusT
    where_ $ callStatus ^. CallStatusCallId ==. val callSid
    return callStatus

findByCallSid' :: L.MonadFlow m => Text -> m (Maybe CallStatus)
findByCallSid' callSid = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamCS.CallStatusT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamCallStatusToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamCS.callId $ Se.Eq callSid]
    Nothing -> pure Nothing

updateCallStatus :: Id CallStatus -> Call.CallStatus -> Int -> Maybe BaseUrl -> SqlDB ()
updateCallStatus callId status conversationDuration mbrecordingUrl = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ CallStatusStatus =. val status,
        CallStatusConversationDuration =. val conversationDuration,
        CallStatusRecordingUrl =. val (showBaseUrl <$> mbrecordingUrl)
      ]
    where_ $ tbl ^. CallStatusId ==. val (getId callId)

updateCallStatus' :: L.MonadFlow m => Id CallStatus -> Call.CallStatus -> Int -> Maybe BaseUrl -> m (MeshResult ())
updateCallStatus' (Id callId) status conversationDuration recordingUrl = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamCS.CallStatusT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
        [ Se.Set BeamCS.conversationDuration conversationDuration,
          Se.Set BeamCS.recordingUrl $ showBaseUrl <$> recordingUrl,
          Se.Set BeamCS.status status
        ]
        [Se.Is BeamCS.callId (Se.Eq callId)]
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

transformBeamCallStatusToDomain :: BeamCS.CallStatus -> CallStatus
transformBeamCallStatusToDomain BeamCS.CallStatusT {..} = do
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

transformDomainCallStatusToBeam :: CallStatus -> BeamCS.CallStatus
transformDomainCallStatusToBeam CallStatus {..} =
  BeamCS.defaultCallStatus
    { BeamCS.id = getId id,
      BeamCS.callId = callId,
      BeamCS.rideId = getId rideId,
      BeamCS.dtmfNumberUsed = dtmfNumberUsed,
      BeamCS.status = status,
      BeamCS.recordingUrl = recordingUrl,
      BeamCS.conversationDuration = conversationDuration,
      BeamCS.createdAt = createdAt
    }
