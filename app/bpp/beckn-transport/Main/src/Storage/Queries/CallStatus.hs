module Storage.Queries.CallStatus where

import Domain.Types.CallStatus
import Domain.Types.Ride
import Kernel.External.Exotel.Types (ExotelCallStatus)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.CallStatus

create :: CallStatus -> SqlDB ()
create = Esq.create

findById :: Transactionable m => Id CallStatus -> m (Maybe CallStatus)
findById = Esq.findById

findByCallSid :: Transactionable m => Text -> m (Maybe CallStatus)
findByCallSid callSid =
  Esq.findOne $ do
    callStatus <- from $ table @CallStatusT
    where_ $ callStatus ^. CallStatusExotelCallSid ==. val callSid
    return callStatus

countCallsByRideId :: Transactionable m => Id Ride -> m Int
countCallsByRideId rideId = (fromMaybe 0 <$>) $
  Esq.findOne $ do
    callStatus <- from $ table @CallStatusT
    where_ $ callStatus ^. CallStatusRideId ==. val (toKey rideId)
    groupBy $ callStatus ^. CallStatusRideId
    pure $ count @Int $ callStatus ^. CallStatusTId

updateCallStatus :: Id CallStatus -> ExotelCallStatus -> Int -> BaseUrl -> SqlDB ()
updateCallStatus callId status conversationDuration recordingUrl = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ CallStatusStatus =. val status,
        CallStatusConversationDuration =. val conversationDuration,
        CallStatusRecordingUrl =. val (Just (showBaseUrl recordingUrl))
      ]
    where_ $ tbl ^. CallStatusId ==. val (getId callId)
