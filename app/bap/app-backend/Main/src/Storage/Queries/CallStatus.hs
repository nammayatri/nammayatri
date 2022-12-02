{-# LANGUAGE TypeApplications #-}

module Storage.Queries.CallStatus where

import Beckn.External.Exotel.Types (ExotelCallStatus)
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.CallStatus
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
