{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSearch where

import qualified Beckn.OnDemand.Transformer.OnSearch as TOnSearch
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.OnDemand.Utils.OnSearch as OnSearchUtils
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import Domain.Types.OnSearchEvent
import qualified Domain.Types.SearchRequest as DSR
import EulerHS.Prelude hiding (find, id, map, readMaybe, state, unpack)
import Kernel.Types.Common hiding (id)
import Kernel.Utils.Common
import qualified Storage.Queries.OnSearchEvent as OnSearchEvent
import Tools.Error

buildOnSearchReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DSR.RiderPreferredOption ->
  Spec.OnSearchReq ->
  m (Maybe DOnSearch.DOnSearchReq)
buildOnSearchReqV2 riderPreferredOption req = do
  logOnSearchEventV2 req
  (_, validTill) <- Utils.getTimestampAndValidTill req.onSearchReqContext
  case req.onSearchReqError of
    Nothing -> do
      message <- req.onSearchReqMessage & fromMaybeM (InvalidRequest "Missing message")
      let catalog = message.onSearchReqMessageCatalog
      providers <- catalog.catalogProviders & fromMaybeM (InvalidRequest "Missing Providers")
      _provider <- safeHead providers & fromMaybeM (InvalidRequest "Missing Provider")
      _fulfillments <- _provider.providerFulfillments & fromMaybeM (InvalidRequest "Missing Fulfillments")
      _items <- _provider.providerItems & fromMaybeM (InvalidRequest "Missing Items")
      fulfillments <- filterM OnSearchUtils.isValidVehVariant _fulfillments
      let items = filter (OnSearchUtils.isValidItem fulfillments) _items
          provider = _provider {Spec.providerFulfillments = Just fulfillments, Spec.providerItems = Just items}
      Just <$> TOnSearch.buildOnSearchReq req provider items fulfillments validTill riderPreferredOption
    Just err -> do
      logTagError "on_search req" $ "on_search error: " <> show err
      pure Nothing

logOnSearchEventV2 :: (EsqDBFlow m r, CacheFlow m r) => Spec.OnSearchReq -> m ()
logOnSearchEventV2 req = do
  let context = req.onSearchReqContext
  createdAt <- getCurrentTime
  id <- generateGUID
  updatedAt <- getCurrentTime
  bppId <- Utils.getContextBppId context
  messageId <- Utils.getMessageIdText context
  (errorCode, errorMessage, errorType) <- case req.onSearchReqError of
    Just err -> do
      let errorCode = err.errorCode
      let errorMessage = err.errorMessage
      let errorType = err.errorMessage
      return (errorCode, errorMessage, errorType)
    Nothing ->
      return (Nothing, Nothing, Nothing)
  void $
    OnSearchEvent.create $
      OnSearchEvent {..}
