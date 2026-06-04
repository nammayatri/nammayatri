{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.SyncSearch (API, handler) where

import qualified Beckn.ACL.Search as ACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Data.Aeson.Text as A
import qualified Data.Text.Lazy as TL
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.SearchRequestProcessing as SRP
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

type API = Search.SyncSearchAPI

handler :: FlowServer API
handler = syncSearch

syncSearch ::
  Text ->
  Maybe Text ->
  Search.SearchReqV2 ->
  FlowHandler Spec.OnSearchReq
syncSearch merchantIdRaw mbToken reqV2 = withFlowHandlerAPI $ do
  let transporterId = Id merchantIdRaw :: Id DM.Merchant
  merchant <- CQM.findById transporterId >>= fromMaybeM (MerchantDoesNotExist transporterId.getId)
  unless (Just merchant.internalApiKey == mbToken) $
    throwError $ AuthBlocked "Invalid sync_search internal api key"
  unless merchant.enabled $ throwError (AgencyDisabled transporterId.getId)
  transactionId <- Utils.getTransactionId reqV2.searchReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logTagInfo "SyncSearchV2 Internal Flow" $ "Reached:-" <> TL.toStrict (A.encodeToLazyText reqV2)
    let context = reqV2.searchReqContext
        txnId = Just transactionId
    bapUri <- Utils.getContextBapUri context
    bapId <- context.contextBapId & fromMaybeM (InvalidRequest "bapId is missing")
    city <- Utils.getContextCity context
    moc <- CQMOC.findByMerchantIdAndCity transporterId city >>= fromMaybeM (InternalError $ "Operating City" <> show city <> "not supported or not found ")
    void $ Utils.validateSearchContext context transporterId moc.id
    dSearchReq <- ACL.buildSearchReqV2Raw bapId bapUri reqV2 bapUri
    msgId <- Utils.getMessageId context
    country <- Utils.getContextCountry context

    isFirst <- Redis.withCrossAppRedis $ Redis.setNxExpire (DSearch.searchTxnDedupKey transactionId transporterId.getId) 60 True
    unless isFirst $
      throwError $ InternalError $ "Search already processed by beckn for txn " <> transactionId
    (_, onSearchReq) <- SRP.processSearchRequest merchant dSearchReq transporterId msgId txnId bapUri city country "sync_search" (toJSON reqV2)
    logTagInfo "SyncSearchV2 Internal Flow" $ "Returning OnSearch inline:-" <> TL.toStrict (A.encodeToLazyText onSearchReq)
    pure onSearchReq
