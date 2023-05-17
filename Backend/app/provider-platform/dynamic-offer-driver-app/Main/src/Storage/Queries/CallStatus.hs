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
import qualified Debug.Trace as T
import Domain.Types.CallStatus as DCS
import Domain.Types.Ride
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import Sequelize as Se
import qualified Sequelize as Se
import qualified Storage.Beam.CallStatus as BeamCS
-- import qualified Storage.Beam.CallStatus as BeamCT
import Storage.Tabular.CallStatus
import qualified Storage.Tabular.CallStatus as CS
import qualified Storage.Tabular.VechileNew as VN

create :: CallStatus -> SqlDB ()
create callStatus = void $ Esq.createUnique callStatus

create' :: L.MonadFlow m => DCS.CallStatus -> m (MeshResult ())
create' callStatus = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' VN.meshConfig (transformDomainCallStatusToBeam callStatus)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findById :: Transactionable m => Id CallStatus -> m (Maybe CallStatus)
findById = Esq.findById

findById' :: L.MonadFlow m => Id CallStatus -> m (Maybe CallStatus)
findById' (Id callStatusId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamCallStatusToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamCS.id $ Se.Eq callStatusId]
    Nothing -> pure Nothing

findByCallSid :: Transactionable m => Text -> m (Maybe CallStatus)
findByCallSid callSid =
  Esq.findOne $ do
    callStatus <- from $ table @CallStatusT
    where_ $ callStatus ^. CallStatusCallId ==. val callSid
    return callStatus

-- findByCallSid' :: Transactionable m => Text -> m (Maybe CallStatus)
-- findByCallSid' callSid =
--   Esq.findOne $ do
--     callStatus <- from $ table @CallStatusT
--     where_ $ callStatus ^. CallStatusCallId ==. val callSid
--     return callStatus

updateCallStatus :: Id CallStatus -> Call.CallStatus -> Int -> BaseUrl -> SqlDB ()
updateCallStatus callId status conversationDuration recordingUrl = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ CallStatusStatus =. val status,
        CallStatusConversationDuration =. val conversationDuration,
        CallStatusRecordingUrl =. val (Just (showBaseUrl recordingUrl))
      ]
    where_ $ tbl ^. CallStatusId ==. val (getId callId)

countCallsByRideId :: Transactionable m => Id Ride -> m Int
countCallsByRideId rideId = (fromMaybe 0 <$>) $
  Esq.findOne $ do
    callStatus <- from $ table @CallStatusT
    where_ $ callStatus ^. CallStatusRideId ==. val (toKey rideId)
    groupBy $ callStatus ^. CallStatusRideId
    pure $ count @Int $ callStatus ^. CallStatusTId

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
