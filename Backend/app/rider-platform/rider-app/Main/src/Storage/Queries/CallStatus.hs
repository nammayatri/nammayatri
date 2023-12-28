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
import Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.CallStatus as BeamCS
import qualified Tools.Call as Call

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => CallStatus -> m ()
create cs = do
  callS <- findByCallSid (cs.callId)
  case callS of
    Nothing -> createWithKV cs
    Just _ -> pure ()

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id CallStatus -> m (Maybe CallStatus)
findById (Id callStatusId) = findOneWithKV [Se.Is BeamCS.id $ Se.Eq callStatusId]

findByCallSid :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> m (Maybe CallStatus)
findByCallSid callSid = findOneWithKV [Se.Is BeamCS.callId $ Se.Eq callSid]

updateCallStatus :: MonadFlow m => Id CallStatus -> Call.CallStatus -> Int -> Maybe Text -> m ()
updateCallStatus (Id callId) status conversationDuration recordingUrl =
  updateWithKV
    [ Se.Set BeamCS.conversationDuration conversationDuration,
      Se.Set BeamCS.recordingUrl recordingUrl,
      Se.Set BeamCS.status status
    ]
    [Se.Is BeamCS.id (Se.Eq callId)]

updateCallStatusInformation :: MonadFlow m => Id CallStatus -> Maybe (Id Ride) -> Maybe Text -> Maybe Call.CallService -> Maybe Text -> m ()
updateCallStatusInformation (Id callStatusId) rideId merchantId callService dtmfNumberUsed =
  updateWithKV
    [ Se.Set BeamCS.merchantId merchantId,
      Se.Set BeamCS.rideId (getId <$> rideId),
      Se.Set BeamCS.callService callService,
      Se.Set BeamCS.dtmfNumberUsed dtmfNumberUsed
    ]
    [Se.Is BeamCS.id (Se.Eq callStatusId)]

updateCallError :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Maybe Text -> Maybe Text -> Maybe Call.CallService -> m ()
updateCallError callSid callError merchantId callService =
  updateWithKV
    [ Se.Set BeamCS.callError callError,
      Se.Set BeamCS.callService callService,
      Se.Set BeamCS.merchantId merchantId
    ]
    [Se.Is BeamCS.callId (Se.Eq callSid)]

instance FromTType' BeamCS.CallStatus CallStatus where
  fromTType' BeamCS.CallStatusT {..} = do
    pure $
      Just
        CallStatus
          { id = Id id,
            callId = callId,
            rideId = Id <$> rideId,
            dtmfNumberUsed = dtmfNumberUsed,
            status = status,
            recordingUrl = recordingUrl,
            conversationDuration = conversationDuration,
            merchantId = merchantId,
            callService = callService,
            callError = callError,
            createdAt = createdAt
          }

instance ToTType' BeamCS.CallStatus CallStatus where
  toTType' CallStatus {..} = do
    BeamCS.CallStatusT
      { BeamCS.id = getId id,
        BeamCS.callId = callId,
        BeamCS.rideId = getId <$> rideId,
        BeamCS.dtmfNumberUsed = dtmfNumberUsed,
        BeamCS.status = status,
        BeamCS.recordingUrl = recordingUrl,
        BeamCS.conversationDuration = conversationDuration,
        BeamCS.merchantId = merchantId,
        BeamCS.callService = callService,
        BeamCS.callError = callError,
        BeamCS.createdAt = createdAt
      }
