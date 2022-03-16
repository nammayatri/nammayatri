module Storage.Queries.CallStatus where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.CallStatus
import Storage.Tabular.CallStatus
import qualified Types.API.Call as CallAPI

create :: CallStatus -> SqlDB ()
create = create'

findById :: Transactionable m => Id CallStatus -> m (Maybe CallStatus)
findById = Esq.findById

updateCallStatus :: Id CallStatus -> CallAPI.CallCallbackReq -> SqlDB ()
updateCallStatus callId req = do
  update' $ \tbl -> do
    set
      tbl
      [ CallStatusStatus =. val req.status,
        CallStatusConversationDuration =. val req.conversationDuration,
        CallStatusRecordingUrl =. val (Just (showBaseUrl req.recordingUrl))
      ]
    where_ $ tbl ^. CallStatusId ==. val (getId callId)
