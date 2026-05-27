{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.OnSearch (API, handler, processOnSearchInline) where

import qualified Beckn.ACL.OnSearch as TaxiACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import Beckn.OnDemand.Utils.OnSearch
import Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified BecknV2.OnDemand.Utils.Common as Utils
import Data.Text as T
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Environment
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Lib.ConfigPilot.Interface.Getter (TxnIdKey (..))
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.SearchRequest as QSearchReq
import TransactionLogs.PushLogs

type API = OnSearch.OnSearchAPIV2

handler :: SignatureAuthResult -> FlowServer API
handler = onSearch

onSearch ::
  SignatureAuthResult ->
  OnSearch.OnSearchReqV2 ->
  FlowHandler AckResponse
onSearch _ reqV2 = withFlowHandlerBecknAPI do
  processOnSearchPayload reqV2 ProcessAsync
  pure Ack

data ProcessingMode = ProcessSync | ProcessAsync

processOnSearchInline :: OnSearch.OnSearchReqV2 -> Flow ()
processOnSearchInline reqV2 = processOnSearchPayload reqV2 ProcessSync

processOnSearchPayload :: OnSearch.OnSearchReqV2 -> ProcessingMode -> Flow ()
processOnSearchPayload reqV2 mode = do
  transactionId <- Utils.getTransactionId reqV2.onSearchReqContext
  L.setOptionLocal TxnIdKey transactionId
  Utils.withTransactionIdLogTag transactionId $ do
    logInfo $ "OnSearch received:-" <> show reqV2
    searchRequest <- runInReplica $ QSearchReq.findById (cast $ Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
    void $ validateOnSearchContext reqV2.onSearchReqContext searchRequest
    mbDOnSearchReq <- TaxiACL.buildOnSearchReqV2 reqV2
    messageId <- Utils.getMessageIdText reqV2.onSearchReqContext

    whenJust mbDOnSearchReq $ \request -> do
      let bppSubId = request.providerInfo.providerId
      Redis.whenWithLockRedis (onSearchLockKey messageId bppSubId) 60 $ do
        validatedRequest <- DOnSearch.validateRequest request searchRequest
        isFirst <- Redis.withCrossAppRedis $ Redis.setNxExpire (onSearchHandledKey transactionId bppSubId) (30 :: Int) (True :: Bool)
        if not isFirst
          then logInfo $ "OnSearch already persisted for txn " <> transactionId <> " subId:" <> bppSubId <> "; skipping duplicate"
          else do
            fork "on search received pushing ondc logs" do
              void $ pushLogs "on_search" (toJSON reqV2) validatedRequest.merchant.id.getId "MOBILITY"
            let runProcessing =
                  Redis.whenWithLockRedis (onSearchProcessingLockKey messageId bppSubId) 60 $
                    DOnSearch.onSearch transactionId validatedRequest
            case mode of
              ProcessAsync -> fork "on search processing" runProcessing
              ProcessSync -> runProcessing

onSearchLockKey :: Text -> Text -> Text
onSearchLockKey msgId bppSubscriberId = "Customer:OnSearch:MessageId-" <> msgId <> "-bppSubscriberId-" <> bppSubscriberId

onSearchProcessingLockKey :: Text -> Text -> Text
onSearchProcessingLockKey msgId bppSubscriberId = "Customer:OnSearch:Processing:MessageId-" <> msgId <> "-bppSubscriberId-" <> bppSubscriberId

onSearchHandledKey :: Text -> Text -> Text
onSearchHandledKey txnId bppSubId = "Customer:OnSearch:Handled:Txn-" <> txnId <> ":bpp-" <> bppSubId
