{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.CallEvent
  ( CallEventReq (..),
    logCallEvent,
    sendCallDataToKafka,
  )
where

import Data.Aeson
import qualified Domain.Types.Ride as Ride
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Event

data CallEventReq = CallEventReq
  { rideId :: Id Ride.Ride,
    exophoneNumber :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

logCallEvent :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EventStreamFlow m r) => CallEventReq -> m APISuccess.APISuccess
logCallEvent CallEventReq {..} = do
  sendCallDataToKafka Nothing (Just rideId) (Just "ANONYMOUS_CALLER") Nothing Nothing User (Just exophoneNumber)
  pure APISuccess.Success

sendCallDataToKafka :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EventStreamFlow m r) => Maybe Text -> Maybe (Id Ride.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> EventTriggeredBy -> Maybe Text -> m ()
sendCallDataToKafka vendor mRideId callType callSid callStatus triggeredBy exophoneNumber = do
  case mRideId of
    Just rideId -> do
      ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
      triggerExophoneEvent $ ExophoneEventData vendor callType (Just rideId) callSid callStatus ride.merchantId triggeredBy (Just ride.driverId) exophoneNumber
    Nothing -> triggerExophoneEvent $ ExophoneEventData vendor callType Nothing callSid callStatus Nothing triggeredBy Nothing exophoneNumber
