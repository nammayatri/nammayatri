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
-- The mirror image of "API.Beckn.FRFS.OnSearch": that module receives an @on_search@
-- callback from a seller we bought from; this one receives the @search@ a buyer sends
-- us. Acknowledge synchronously, then do the work — including the outbound
-- @on_search@ — on a fork.
module API.Beckn.FRFSSeller.Search
  ( API,
    handler,
    searchDedupeKey,
  )
where

import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Domain.Action.Beckn.FRFSSeller.Search as DSearch
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = Spec.SearchAPI

handler :: SignatureAuthResult -> FlowServer API
handler = search

-- | One claim per (transaction, message).
--
-- The gateway may fan a single search to several of our registered subscriber URIs;
-- without this, each delivery emits its own @on_search@ and buyers NACK the duplicate
-- as out-of-sequence.
searchDedupeKey :: Text -> Text -> Text
searchDedupeKey txnId msgId = "frfsSeller:search:" <> txnId <> ":" <> msgId

searchDedupeTtlSeconds :: Redis.ExpirationTime
searchDedupeTtlSeconds = 60

search :: SignatureAuthResult -> Spec.SearchReq -> FlowHandler Spec.AckResponse
search _authResult req = withFlowHandlerAPI $ do
  transactionId <-
    req.searchReqContext.contextTransactionId
      & fromMaybeM (InvalidRequest "TransactionId not found")
  messageId <-
    req.searchReqContext.contextMessageId
      & fromMaybeM (InvalidRequest "MessageId not found")
  withTransactionIdLogTag' transactionId $ do
    -- Fail open: with Redis unavailable we would rather emit a duplicate on_search
    -- than answer nothing at all.
    isFirst <-
      try @_ @SomeException
        ( Redis.withCrossAppRedis $
            Redis.setNxExpire (searchDedupeKey transactionId messageId) searchDedupeTtlSeconds True
        )
        >>= \case
          Right claimed -> pure claimed
          Left err -> do
            logWarning $ "FRFS seller search dedupe unavailable, processing anyway: " <> show err
            pure True
    if isFirst
      then do
        logInfo $ "FRFS seller search accepted: msg=" <> messageId
        fork "FRFS seller on_search processing" $ DSearch.handleSearch req
      else logInfo $ "FRFS seller search duplicate ignored: msg=" <> messageId
  pure Utils.ack
