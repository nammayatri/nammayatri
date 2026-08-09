{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Inbound @search@ from a buyer app, answered as an ONDC:TRV11 SELLER.
--
-- This is the mirror image of "API.Beckn.FRFS.OnSearch": that module receives an
-- @on_search@ callback from a seller we bought from; this one receives the
-- @search@ request a buyer sends us. Phase 1 acknowledges and does nothing else —
-- the catalog and the outbound @on_search@ callback arrive in later tasks.
module API.Beckn.FRFSSeller.Search
  ( API,
    handler,
  )
where

import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Environment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = Spec.SearchAPI

handler :: SignatureAuthResult -> FlowServer API
handler = search

search :: SignatureAuthResult -> Spec.SearchReq -> FlowHandler Spec.AckResponse
search _authResult req = withFlowHandlerAPI $ do
  transactionId <-
    req.searchReqContext.contextTransactionId
      & fromMaybeM (InvalidRequest "TransactionId not found")
  messageId <-
    req.searchReqContext.contextMessageId
      & fromMaybeM (InvalidRequest "MessageId not found")
  withTransactionIdLogTag' transactionId $
    logInfo $ "FRFS seller search received: msg=" <> messageId
  pure Utils.ack
