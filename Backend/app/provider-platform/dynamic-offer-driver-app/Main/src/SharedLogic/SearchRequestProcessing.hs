{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SearchRequestProcessing
  ( processSearchRequest,
  )
where

import qualified Beckn.ACL.OnSearch as ACL
import qualified BecknV2.OnDemand.Types as Spec
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.ValueAddNP as VNP
import TransactionLogs.PushLogs

-- | Shared search processing used by both the async Beckn search handler
-- and the synchronous internal sync_search endpoint.
-- Validates the request, pushes ONDC logs, runs DSearch.handler,
-- filters quotes based on isValueAddNP, and builds the OnSearch response.
processSearchRequest ::
  DM.Merchant ->
  DSearch.DSearchReq ->
  Id DM.Merchant ->
  Text ->
  Maybe Text ->
  BaseUrl ->
  Context.City ->
  Context.Country ->
  Text ->
  Value ->
  Flow (DSearch.DSearchRes, Spec.OnSearchReq)
processSearchRequest merchant dSearchReq transporterId msgId txnId bapUri city country logTag reqJson = do
  validatedSReq <- DSearch.validateRequest merchant dSearchReq
  fork (logTag <> " received pushing ondc logs") $
    void $ pushLogs logTag reqJson validatedSReq.merchant.id.getId "MOBILITY"
  let bppId = validatedSReq.merchant.subscriberId.getShortId
  bppUri <- Utils.mkBppUri transporterId.getId
  dSearchResWithQuotes <- DSearch.handler validatedSReq dSearchReq
  isValueAddNP <- VNP.isValueAddNP dSearchReq.bapId
  let dSearchResWihoutQuotes = dSearchResWithQuotes {DSearch.quotes = []}
      dSearchRes = bool dSearchResWihoutQuotes dSearchResWithQuotes isValueAddNP
  onSearchReq <- ACL.mkOnSearchRequest dSearchRes Context.ON_SEARCH Context.MOBILITY msgId txnId bapUri (Just bppId) (Just bppUri) city country isValueAddNP
  pure (dSearchRes, onSearchReq)
