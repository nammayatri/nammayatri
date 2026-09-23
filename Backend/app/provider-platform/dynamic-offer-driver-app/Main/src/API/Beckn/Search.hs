{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Search (API, handler) where

import qualified Beckn.ACL.Search as ACL
import qualified Beckn.OnDemand.Transformer.OndcScheduledRide.OnSearch as OSROnSearch
import qualified Beckn.OnDemand.Transformer.OndcScheduledRide.Search as OSRSearch
import qualified Beckn.OnDemand.Utils.OndcScheduledRide.Common as OSRCommon
import qualified Beckn.OnDemand.Utils.Callback as Callback
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Data.Aeson.Text as A
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Merchant as DM
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as ET
import Kernel.Beam.Types (TxnIdKey (..))
import Kernel.External.BapHostRedirect (shouldRedirectBapHost)
import qualified Kernel.Prelude as Kernel
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Error
import qualified Kernel.Types.Error as KernelError
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified Kernel.Utils.SignatureAuth as HttpSig
import Servant hiding (throwError)
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.GatewayDispatch as GatewayDispatch
import qualified SharedLogic.SearchRequestProcessing as SRP
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BapMetadata as CQBapMetaData
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Tools.ActorInfo as ActorInfo

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Header "X-Gateway-Authorization" Text
    :> Header "Beckn-Body-Hash" Text
    :> Search.SearchAPI

handler :: FlowServer API
handler = search

forwardSearchToBpp ::
  BaseUrl ->
  Id DM.Merchant ->
  SignatureAuthResult ->
  Maybe SignatureAuthResult ->
  Search.SearchReqV2 ->
  Flow AckResponse
forwardSearchToBpp redirectBaseUrl merchantId authResult mbGatewayAuthResult reqV2 = do
  gatewayAuthResult <- mbGatewayAuthResult & fromMaybeM (InternalError "Cannot forward search: no verified gateway signature")
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
  withShortRetry $
    callAPI redirectedUrl clientWithHeaders "search" Search.searchAPI
      >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_FORWARD_SEARCH") redirectedUrl)

-- | Verifying: the Gateway signature is required and fully verified as
-- normal for everyone. It's only skipped when both (a) the header is
-- entirely absent and (b) the BAP (already verified via Authorization) is a
-- whitelisted noSignatureSubscribers test subscriber -- exactly ONDC
-- Workbench's direct-to-BPP test mode, which never simulates the Gateway
-- hop and so never sends this header at all. Any BAP not on that list still
-- gets the same MissingHeader failure as before.
verifyOrSkipGatewayAuth ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Maybe Text ->
  Maybe Text ->
  Flow (Maybe SignatureAuthResult)
verifyOrSkipGatewayAuth transporterId authResult mbGatewayAuthHeader mbBodyHashHeader =
  case mbGatewayAuthHeader of
    Just gatewayAuthHeader -> do
      result <- authCheck "X-Gateway-Authorization" (Just (encodeUtf8 gatewayAuthHeader)) (encodeUtf8 <$> mbBodyHashHeader) transporterId.getId Subscriber.BG Domain.MOBILITY
      pure (Just result)
    Nothing -> do
      noSignatureSubscribers <- asks (.noSignatureSubscribers)
      if authResult.subscriber.subscriber_id `elem` noSignatureSubscribers
        then pure Nothing
        else throwError (KernelError.MissingHeader "X-Gateway-Authorization")

search ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Maybe Text ->
  Maybe Text ->
  Search.SearchReqV2 ->
  FlowHandler AckResponse
search transporterId authResult mbGatewayAuthHeader mbBodyHashHeader reqV2 = withFlowHandlerBecknAPI . ActorInfo.withRequestIdActorInfo $ do
  mbGatewayAuthResult <- verifyOrSkipGatewayAuth transporterId authResult mbGatewayAuthHeader mbBodyHashHeader
  bapUri <- Utils.getContextBapUri reqV2.searchReqContext
  redirectMap <- asks (.bapHostRedirectMap)
  case shouldRedirectBapHost redirectMap bapUri of
    Just (Just url) -> forwardSearchToBpp url transporterId authResult mbGatewayAuthResult reqV2
    _ -> do
      -- Process locally
      transactionId <- Utils.getTransactionId reqV2.searchReqContext
      L.setOptionLocal TxnIdKey transactionId
      Utils.withTransactionIdLogTag transactionId $ do
        logTagInfo "SearchV2 API Flow Local Processing" $ "Reached:-" <> TL.toStrict (A.encodeToLazyText reqV2)
        let context = reqV2.searchReqContext
            txnId = Just transactionId
        city <- Utils.getContextCity context
        merchant <- CQM.findById transporterId >>= fromMaybeM (MerchantDoesNotExist transporterId.getId)
        unless merchant.enabled $ throwError (AgencyDisabled transporterId.getId)
        moc <- CQMOC.findByMerchantIdAndCity transporterId city >>= fromMaybeM (InvalidRequest $ "Operating City " <> show city <> " not supported or not found")
        void $ Utils.validateSearchContext context transporterId moc.id
        dSearchReq' <- ACL.buildSearchReqV2 authResult.subscriber reqV2 bapUri
        msgId <- Utils.getMessageId context
        country <- Utils.getContextCountry context

        mbBapMetadata' <- CQBapMetaData.findBySubscriberIdDomainMerchantAndCity (Id dSearchReq'.bapId) Domain.MOBILITY merchant.id moc.id
        let isOndcScheduledRideSupportEnabled = fromMaybe False (mbBapMetadata' >>= (.enableOndcScheduledRideSupport))
        (dSearchReq, mbBapMetadata) <-
          if isOndcScheduledRideSupportEnabled
            then do
              -- Pilot BAPs get isSchedule derived from the category code and their STATIC_TERMS verified/stored, in one pass.
              updatedBapMetadata <- OSRCommon.verifyIncomingStaticTerms (Id dSearchReq'.bapId) Domain.MOBILITY merchant.id moc.id mbBapMetadata' (reqV2.searchReqMessage.searchReqMessageIntent >>= (.intentTags))
              pure (OSRSearch.ondcScheduledRideParser reqV2.searchReqMessage dSearchReq', updatedBapMetadata)
            else pure (dSearchReq', mbBapMetadata')
        DSearch.validateScheduledBookingWindowForSearch moc.id dSearchReq

        isFirst <- Redis.withCrossAppRedis $ Redis.setNxExpire (DSearch.searchTxnDedupKey transactionId transporterId.getId) 60 True
        when isFirst $
          Redis.whenWithLockRedis (searchLockKey dSearchReq.messageId transporterId.getId) 60 $
            fork "search request processing" $
              Redis.whenWithLockRedis (searchProcessingLockKey dSearchReq.messageId transporterId.getId) 60 $ do
                (dSearchRes, onSearchReq') <- SRP.processSearchRequest merchant dSearchReq transporterId msgId txnId bapUri city country "search" (toJSON reqV2)
                -- Same pilot check, patches the already-built on_search reply's catalog.tags with BPP_TERMS.
                onSearchReq <-
                  if isOndcScheduledRideSupportEnabled
                    then OSROnSearch.ondcScheduledRideOnSearchMessageBuild merchant.id moc.id mbBapMetadata dSearchRes onSearchReq'
                    else pure onSearchReq'
                internalEndPointHashMap <- asks (.internalEndPointHashMap)
                let context' = onSearchReq.onSearchReqContext
                logTagInfo "SearchV2 API Flow" $ "Sending OnSearch:-" <> TL.toStrict (A.encodeToLazyText onSearchReq)
                void $
                  GatewayDispatch.dispatchAction dSearchRes.provider.id
                    Domain.MOBILITY
                    "on_search"
                    (onSearchReq.onSearchReqContext.contextBapId)
                    onSearchReq
                    (Callback.withCallback dSearchRes.provider "on_search" OnSearch.onSearchAPIV2 bapUri internalEndPointHashMap (errHandler context') $ pure onSearchReq)
                    (\url mappedAction jsonBody -> withShortRetry $ CallBAP.callBecknAPIUnsigned mappedAction url jsonBody)
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
