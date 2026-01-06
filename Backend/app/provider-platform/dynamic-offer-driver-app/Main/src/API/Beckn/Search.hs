{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Search (API, handler) where

import qualified Beckn.ACL.OnSearch as ACL
import qualified Beckn.ACL.Search as ACL
import qualified Beckn.OnDemand.Utils.Callback as Callback
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Data.Aeson.Text as A
import Data.List.Extra (notNull)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import Kernel.External.BapHostRedirect (shouldRedirectBapHost)
import qualified Kernel.Prelude as Kernel
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified Kernel.Utils.SignatureAuth as HttpSig
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.ValueAddNP as VNP
import TransactionLogs.PushLogs

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> SignatureAuth 'Domain.MOBILITY "X-Gateway-Authorization"
    :> Search.SearchAPI

handler :: FlowServer API
handler = search

forwardSearchToBpp ::
  BaseUrl ->
  Id DM.Merchant ->
  SignatureAuthResult ->
  SignatureAuthResult ->
  Search.SearchReqV2 ->
  Flow AckResponse
forwardSearchToBpp redirectBaseUrl merchantId authResult gatewayAuthResult reqV2 = do
  let basePath = Kernel.baseUrlPath redirectBaseUrl
      becknPath = basePath <> "/beckn/" <> T.unpack merchantId.getId
      redirectedUrl = redirectBaseUrl {Kernel.baseUrlPath = becknPath}
  logInfo $ "Forwarding to " <> Kernel.showBaseUrl redirectedUrl <> " for merchant " <> merchantId.getId
  let baseClient = ET.client Search.searchAPI reqV2
      gatewaySignature = decodeUtf8 $ HttpSig.encode gatewayAuthResult.signature
      clientWithHeaders =
        withHeaders
          [ ("Authorization", decodeUtf8 $ HttpSig.encode authResult.signature),
            ("X-Gateway-Authorization", gatewaySignature),
            ("Proxy-Authorization", gatewaySignature)
          ]
          baseClient
  callAPI redirectedUrl clientWithHeaders "search" Search.searchAPI
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_FORWARD_SEARCH") redirectedUrl)

search ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  SignatureAuthResult ->
  Search.SearchReqV2 ->
  FlowHandler AckResponse
search transporterId authResult gatewayAuthResult reqV2 = withFlowHandlerBecknAPI $ do
  bapUri <- Utils.getContextBapUri reqV2.searchReqContext
  redirectMap <- asks (.bapHostRedirectMap)
  case shouldRedirectBapHost redirectMap bapUri of
    Just (Just url) -> forwardSearchToBpp url transporterId authResult gatewayAuthResult reqV2
    _ -> do
      -- Process locally
      transactionId <- Utils.getTransactionId reqV2.searchReqContext
      Utils.withTransactionIdLogTag transactionId $ do
        logTagInfo "SearchV2 API Flow Local Processing" $ "Reached:-" <> TL.toStrict (A.encodeToLazyText reqV2)
        let context = reqV2.searchReqContext
            txnId = Just transactionId
        city <- Utils.getContextCity context
        merchant <- CQM.findById transporterId >>= fromMaybeM (MerchantDoesNotExist transporterId.getId)
        unless merchant.enabled $ throwError (AgencyDisabled transporterId.getId)
        moc <- CQMOC.findByMerchantIdAndCity transporterId city >>= fromMaybeM (InternalError $ "Operating City" <> show city <> "not supported or not found ")
        void $ Utils.validateSearchContext context transporterId moc.id
        dSearchReq <- ACL.buildSearchReqV2 authResult.subscriber reqV2
        msgId <- Utils.getMessageId context
        country <- Utils.getContextCountry context

        Redis.whenWithLockRedis (searchLockKey dSearchReq.messageId transporterId.getId) 60 $ do
          validatedSReq <- DSearch.validateRequest merchant dSearchReq
          fork "search received pushing ondc logs" do
            void $ pushLogs "search" (toJSON reqV2) validatedSReq.merchant.id.getId "MOBILITY"
          let bppId = validatedSReq.merchant.subscriberId.getShortId
          bppUri <- Utils.mkBppUri transporterId.getId
          fork "search request processing" $
            Redis.whenWithLockRedis (searchProcessingLockKey dSearchReq.messageId transporterId.getId) 60 $ do
              dSearchResWithQuotes <- DSearch.handler validatedSReq dSearchReq
              internalEndPointHashMap <- asks (.internalEndPointHashMap)

              isValueAddNP <- VNP.isValueAddNP dSearchReq.bapId
              let dSearchResWihoutQuotes = dSearchResWithQuotes {DSearch.quotes = []}
              -- in case of non value-add-np transactions, when quotes are present, setting them empty to avoid sending quotes to BAP.
              let dSearchRes = bool dSearchResWihoutQuotes dSearchResWithQuotes isValueAddNP

              when ((notNull dSearchRes.quotes && isValueAddNP) || null dSearchRes.quotes) $ do
                onSearchReq <- ACL.mkOnSearchRequest dSearchRes Context.ON_SEARCH Context.MOBILITY msgId txnId bapUri (Just bppId) (Just bppUri) city country isValueAddNP
                let context' = onSearchReq.onSearchReqContext
                logTagInfo "SearchV2 API Flow" $ "Sending OnSearch:-" <> TL.toStrict (A.encodeToLazyText onSearchReq)
                void $
                  Callback.withCallback dSearchRes.provider "on_search" OnSearch.onSearchAPIV2 bapUri internalEndPointHashMap (errHandler context') $ do
                    pure onSearchReq
        pure Ack

searchLockKey :: Text -> Text -> Text
searchLockKey id mId = "Driver:Search:MessageId-" <> id <> ":" <> mId

searchProcessingLockKey :: Text -> Text -> Text
searchProcessingLockKey id mId = "Driver:Search:Processing:MessageId-" <> id <> ":" <> mId

errHandler :: Spec.Context -> BecknAPIError -> Spec.OnSearchReq
errHandler context (BecknAPIError err) =
  Spec.OnSearchReq
    { onSearchReqContext = context,
      onSearchReqError = Just err',
      onSearchReqMessage = Nothing
    }
  where
    err' =
      Spec.Error
        { errorCode = Just err.code,
          errorMessage = err.message >>= \m -> Just $ encodeToText err._type <> " " <> m,
          errorPaths = err.path
        }
