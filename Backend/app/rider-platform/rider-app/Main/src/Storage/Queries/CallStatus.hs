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
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.CallStatus
import qualified Tools.Call as Call

create :: CallStatus -> SqlDB ()
create callStatus = void $ Esq.createUnique callStatus

findById :: Transactionable m => Id CallStatus -> m (Maybe CallStatus)
findById = Esq.findById

findByCallSid :: Transactionable m => Text -> m (Maybe CallStatus)
findByCallSid callSid =
  Esq.findOne $ do
    callStatus <- from $ table @CallStatusT
    where_ $ callStatus ^. CallStatusCallId ==. val callSid
    return callStatus

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
