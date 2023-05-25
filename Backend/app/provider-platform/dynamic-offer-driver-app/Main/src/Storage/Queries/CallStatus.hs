{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.CallStatus where

import qualified Data.Text as T
import Database.Beam (insertExpressions)
import qualified Debug.Trace as T
import Domain.Types.CallStatus
import Domain.Types.Ride
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Sequelize as Se
import qualified Storage.Beam.CallStatus as BeamCT
import Storage.Tabular.CallStatus
import qualified Storage.Tabular.CallStatus as CS
import qualified Storage.Tabular.VechileNew as VN

-- create :: CallStatus -> SqlDB ()
-- create callStatus = void $ Esq.createUnique callStatus

-- create' :: CallStatus -> SqlDB ()
create :: L.MonadFlow m => CallStatus -> m (MeshResult ())
create callStatus = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' VN.meshConfig (transformDomainCallStatusToBeam callStatus)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findById :: Transactionable m => Id CallStatus -> m (Maybe CallStatus)
-- findById = Esq.findById

findById :: L.MonadFlow m => Id CallStatus -> m (Maybe CallStatus)
findById (Id callStatusId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamCallStatusToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamCT.id $ Se.Eq callStatusId]
    Nothing -> pure Nothing

-- findByCallSid :: Transactionable m => Text -> m (Maybe CallStatus)
-- findByCallSid callSid =
--   Esq.findOne $ do
--     callStatus <- from $ table @CallStatusT
--     where_ $ callStatus ^. CallStatusCallId ==. val callSid
--     return callStatus

findByCallSid :: L.MonadFlow m => Text -> m (Maybe CallStatus)
findByCallSid callSid = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamCallStatusToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamCT.callId $ Se.Eq callSid]
    Nothing -> pure Nothing

-- updateCallStatus :: Id CallStatus -> Call.CallStatus -> Int -> BaseUrl -> SqlDB ()
-- updateCallStatus callId status conversationDuration recordingUrl = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ CallStatusStatus =. val status,
--         CallStatusConversationDuration =. val conversationDuration,
--         CallStatusRecordingUrl =. val (Just (showBaseUrl recordingUrl))
--       ]
--     where_ $ tbl ^. CallStatusId ==. val (getId callId)

-- updateCallStatus' :: Id CallStatus -> Call.CallStatus -> Int -> BaseUrl -> SqlDB ()
updateCallStatus :: L.MonadFlow m => Id CallStatus -> Call.CallStatus -> Int -> BaseUrl -> m (MeshResult ())
updateCallStatus (Id callId) status conversationDuration recordingUrl = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Set BeamCT.conversationDuration conversationDuration,
          Set BeamCT.recordingUrl $ Just (showBaseUrl recordingUrl),
          Set BeamCT.status status
        ]
        [Is BeamCT.callId (Se.Eq callId)]
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

countCallsByRideId :: Transactionable m => Id Ride -> m Int
countCallsByRideId rideId = (fromMaybe 0 <$>) $
  Esq.findOne $ do
    callStatus <- from $ table @CallStatusT
    where_ $ callStatus ^. CallStatusRideId ==. val (toKey rideId)
    groupBy $ callStatus ^. CallStatusRideId
    pure $ count @Int $ callStatus ^. CallStatusTId

transformBeamCallStatusToDomain :: BeamCT.CallStatus -> CallStatus
transformBeamCallStatusToDomain BeamCT.CallStatusT {..} = do
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

transformDomainCallStatusToBeam :: CallStatus -> BeamCT.CallStatus
transformDomainCallStatusToBeam CallStatus {..} =
  BeamCT.CallStatusT
    { BeamCT.id = getId id,
      BeamCT.callId = callId,
      BeamCT.rideId = getId rideId,
      BeamCT.dtmfNumberUsed = dtmfNumberUsed,
      BeamCT.status = status,
      BeamCT.recordingUrl = recordingUrl,
      BeamCT.conversationDuration = conversationDuration,
      BeamCT.createdAt = createdAt
    }
