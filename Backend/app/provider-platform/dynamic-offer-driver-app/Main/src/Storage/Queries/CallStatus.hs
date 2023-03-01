{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.CallStatus where

import Domain.Types.CallStatus
import Domain.Types.Ride
import Kernel.External.Exotel.Types (ExotelCallStatus)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.CallStatus

create :: CallStatus -> SqlDB m ()
create = Esq.create

findById :: forall m ma. Transactionable ma m => Proxy ma -> Id CallStatus -> m (Maybe CallStatus)
findById _ = Esq.findById @m @ma

findByCallSid :: forall m ma. Transactionable ma m => Text -> Proxy ma -> m (Maybe CallStatus)
findByCallSid callSid _ =
  Esq.findOne @m @ma $ do
    callStatus <- from $ table @CallStatusT
    where_ $ callStatus ^. CallStatusExotelCallSid ==. val callSid
    return callStatus

updateCallStatus :: Id CallStatus -> ExotelCallStatus -> Int -> BaseUrl -> SqlDB m ()
updateCallStatus callId status conversationDuration recordingUrl = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ CallStatusStatus =. val status,
        CallStatusConversationDuration =. val conversationDuration,
        CallStatusRecordingUrl =. val (Just (showBaseUrl recordingUrl))
      ]
    where_ $ tbl ^. CallStatusId ==. val (getId callId)

countCallsByRideId :: forall m ma. Transactionable ma m => Id Ride -> Proxy ma -> m Int
countCallsByRideId rideId _ = (fromMaybe 0 <$>) $
  Esq.findOne @m @ma $ do
    callStatus <- from $ table @CallStatusT
    where_ $ callStatus ^. CallStatusRideId ==. val (toKey rideId)
    groupBy $ callStatus ^. CallStatusRideId
    pure $ count @Int $ callStatus ^. CallStatusTId
