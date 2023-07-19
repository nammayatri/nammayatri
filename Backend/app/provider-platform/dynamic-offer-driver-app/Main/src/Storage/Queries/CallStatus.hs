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
import Database.Beam.Postgres
import Domain.Types.CallStatus
import Domain.Types.Ride
import EulerHS.KVConnector.Utils (meshModelTableEntity)
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import qualified Kernel.External.Call.Interface.Types as Call
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findOneWithKV, updateWithKV)
import Sequelize as Se
import qualified Storage.Beam.CallStatus as BeamCT

create :: (L.MonadFlow m, Log m) => CallStatus -> m ()
create = createWithKV

findById :: (L.MonadFlow m, Log m) => Id CallStatus -> m (Maybe CallStatus)
findById (Id callStatusId) = findOneWithKV [Se.Is BeamCT.id $ Se.Eq callStatusId]

findByCallSid :: (L.MonadFlow m, Log m) => Text -> m (Maybe CallStatus)
findByCallSid callSid = findOneWithKV [Se.Is BeamCT.callId $ Se.Eq callSid]

updateCallStatus :: (L.MonadFlow m, Log m) => Id CallStatus -> Call.CallStatus -> Int -> Maybe BaseUrl -> m ()
updateCallStatus (Id callId) status conversationDuration recordingUrl =
  updateWithKV
    [ Set BeamCT.conversationDuration conversationDuration,
      Set BeamCT.recordingUrl $ showBaseUrl <$> recordingUrl,
      Set BeamCT.status status
    ]
    [Is BeamCT.callId (Se.Eq callId)]

-- countCallsByRideId :: Transactionable m => Id Ride -> m Int
-- countCallsByRideId rideId = (fromMaybe 0 <$>) $
--   Esq.findOne $ do
--     callStatus <- from $ table @CallStatusT
--     where_ $ callStatus ^. CallStatusRideId ==. val (toKey rideId)
--     groupBy $ callStatus ^. CallStatusRideId
--     pure $ count @Int $ callStatus ^. CallStatusTId

-- to be discussed if it should be a beam query or not
countCallsByRideId :: L.MonadFlow m => Id Ride -> m Int
countCallsByRideId rideID = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  conn <- L.getOrInitSqlConn (fromJust dbConf)
  case conn of
    Right c -> do
      resp <-
        L.runDB c $
          L.findRow $
            B.select $
              B.aggregate_ (\ride -> (B.group_ (BeamCT.rideId ride), B.as_ @Int B.countAll_)) $
                B.filter_' (\(BeamCT.CallStatusT {..}) -> rideId B.==?. B.val_ (getId rideID)) $
                  B.all_ (meshModelTableEntity @BeamCT.CallStatusT @Postgres @(DatabaseWith BeamCT.CallStatusT))
      case resp of
        Right (Just resp') -> pure (snd resp')
        _ -> pure 0
    Left _ -> pure 0

instance FromTType' BeamCT.CallStatus CallStatus where
  fromTType' BeamCT.CallStatusT {..} = do
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

instance ToTType' BeamCT.CallStatus CallStatus where
  toTType' CallStatus {..} = do
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
