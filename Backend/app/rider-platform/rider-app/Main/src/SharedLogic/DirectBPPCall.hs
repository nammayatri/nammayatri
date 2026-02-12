{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

-- | Direct BPP call functions for in-process communication.
-- These functions bypass the ONDC Beckn HTTP protocol and call BPP domain
-- handlers directly for valueAddedNP co-located deployments.
-- Each function: pushes ONDC logs, calls BPP handler via withBPPFlow,
-- processes any callback response in BAP context.
module SharedLogic.DirectBPPCall
  ( directSearch,
    directSelect,
    directInit,
    directConfirm,
    directCancel,
    directStatus,
    directUpdate,
    directTrack,
    directRating,
  )
where

import qualified "dynamic-offer-driver-app" Beckn.ACL.OnSearch as BPPOnSearchACL
import qualified "dynamic-offer-driver-app" Beckn.ACL.Search as BPPSearchACL
import qualified "dynamic-offer-driver-app" Domain.Action.Beckn.Search as BPPSearch
import qualified "dynamic-offer-driver-app" SharedLogic.DirectHandler as BPPDirect
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.Merchant as BPPCQM
import qualified "dynamic-offer-driver-app" Storage.CachedQueries.Merchant.MerchantOperatingCity as BPPCQMOC
import qualified Beckn.ACL.OnSearch as BAPOnSearchACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import Beckn.Types.Core.Taxi.API.Cancel as CancelAPI
import Beckn.Types.Core.Taxi.API.Confirm as ConfirmAPI
import qualified Beckn.Types.Core.Taxi.API.Init as InitAPI
import Beckn.Types.Core.Taxi.API.Rating as RatingAPI
import qualified Beckn.Types.Core.Taxi.API.Search as SearchAPI
import Beckn.Types.Core.Taxi.API.Select as SelectAPI
import Beckn.Types.Core.Taxi.API.Status as StatusAPI
import Beckn.Types.Core.Taxi.API.Track as TrackAPI
import Beckn.Types.Core.Taxi.API.Update as UpdateAPI
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Domain.Action.Beckn.OnSearch as BAPOnSearch
import qualified Domain.Types.Merchant as Merchant
import Environment
import qualified EulerHS.Runtime as R
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.BPPFlowRunner (withBPPFlow, withDirectBPP)
import qualified SharedLogic.DirectBAPCallback as DirectBAPCallback
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.SearchRequest as QSearchReq
import qualified Tools.Metrics as Metrics
import TransactionLogs.PushLogs
import qualified Beckn.ACL.Cancel as ACLCancel
import qualified Beckn.ACL.Confirm as ACLConfirm
import qualified Beckn.ACL.OnInit as ACLOnInit
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.CancellationReason (CancellationReasonCode (..), CancellationStage (..))
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError

-- | Extract BPP merchant ID from Beckn context.
getBppMerchantId :: MonadFlow m => Maybe Text -> m Text
getBppMerchantId = fromMaybeM (InvalidRequest "BppId missing in context")

-- | Direct search bypassing Beckn protocol for valueAddedNP.
-- Calls BPP domain handler directly and processes on_search result in BAP.
directSearch ::
  R.FlowRuntime ->
  SearchAPI.SearchReqV2 ->
  Id Merchant.Merchant ->
  Flow ()
directSearch bppFlowRt becknSearchReq merchantId = do
  -- Push ONDC log for outgoing search request
  fork "pushing ondc search logs" $
    void $ pushLogs "search" (toJSON becknSearchReq) merchantId.getId "MOBILITY"

  -- Extract context info
  transactionId <- Utils.getTransactionId becknSearchReq.searchReqContext
  bapUri <- Utils.getContextBapUri becknSearchReq.searchReqContext
  msgId <- Utils.getMessageId becknSearchReq.searchReqContext
  city <- Utils.getContextCity becknSearchReq.searchReqContext
  country <- Utils.getContextCountry becknSearchReq.searchReqContext

  -- Run BPP domain logic via withBPPFlow
  (dSearchRes, bppId, bppUri, isVANP) <- withBPPFlow bppFlowRt $ do
    let bapId = becknSearchReq.searchReqContext.contextBapId
    bapIdText <- fromMaybeM (InvalidRequest "BapId missing") bapId
    bppMerchantId <- getBppMerchantId becknSearchReq.searchReqContext.contextBppId
    merchant <- BPPCQM.findById (Id bppMerchantId) >>= fromMaybeM (MerchantDoesNotExist bppMerchantId)
    _moc <- BPPCQMOC.findByMerchantIdAndCity (Id bppMerchantId) city >>= fromMaybeM (InternalError "Operating City not found")

    -- BPP search ACL needs a subscriber but only uses subscriber_id for validation.
    -- For direct calls, subscriber is constructed by BPP's DirectHandler.mkSubscriberForDirectCall.
    -- Here we call BPP search ACL directly since search has a unique flow.
    subscriber <- BPPDirect.mkSubscriberForDirectCall becknSearchReq.searchReqContext
    dSearchReq <- BPPSearchACL.buildSearchReqV2 subscriber becknSearchReq bapUri

    validatedSReq <- BPPSearch.validateRequest merchant dSearchReq
    dSearchRes <- BPPSearch.handler validatedSReq dSearchReq

    let isVANP = True
    let bppSubscriberId = merchant.subscriberId.getShortId
    bppUri' <- Utils.mkBppUri (Id bppMerchantId).getId
    pure (dSearchRes, Just bppSubscriberId, Just bppUri', isVANP)

  -- Construct Beckn V2 on_search response in BPP context
  onSearchReq <- withBPPFlow bppFlowRt $
    BPPOnSearchACL.mkOnSearchRequest
      dSearchRes
      Context.ON_SEARCH
      Context.MOBILITY
      msgId
      (Just transactionId)
      bapUri
      bppId
      bppUri
      city
      country
      isVANP

  -- Push ONDC log for on_search callback
  fork "pushing ondc on_search logs" $
    void $ pushLogs "on_search" (toJSON onSearchReq) merchantId.getId "MOBILITY"

  -- Process on_search in BAP domain
  mbDOnSearchReq <- BAPOnSearchACL.buildOnSearchReqV2 onSearchReq
  whenJust mbDOnSearchReq $ \request -> do
    searchRequest <- runInReplica $ QSearchReq.findById (cast $ Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
    validatedRequest <- BAPOnSearch.validateRequest request searchRequest
    fork "direct on_search processing" $
      BAPOnSearch.onSearch transactionId validatedRequest

-- | Direct select. Fire-and-forget.
-- Driver offers arrive asynchronously via CallBAP (DirectBAPCallback).
directSelect ::
  R.FlowRuntime ->
  SelectAPI.SelectReqV2 ->
  Id Merchant.Merchant ->
  Flow ()
directSelect bppFlowRt reqV2 merchantId = do
  fork "pushing ondc select logs" $
    void $ pushLogs "select" (toJSON reqV2) merchantId.getId "MOBILITY"

  bppMerchantId <- getBppMerchantId reqV2.selectReqContext.contextBppId
  withBPPFlow bppFlowRt $ BPPDirect.handleSelect (Id bppMerchantId) reqV2

-- | Direct init. Returns on_init response and processes in BAP.
directInit ::
  R.FlowRuntime ->
  InitAPI.InitReqV2 ->
  Id Merchant.Merchant ->
  Flow ()
directInit bppFlowRt reqV2 merchantId = do
  fork "pushing ondc init logs" $
    void $ pushLogs "init" (toJSON reqV2) merchantId.getId "MOBILITY"

  bppMerchantId <- getBppMerchantId reqV2.initReqContext.contextBppId
  mbOnInitReq <- withBPPFlow bppFlowRt $ BPPDirect.handleInit (Id bppMerchantId) reqV2

  -- Push ONDC log and process on_init response in BAP
  whenJust mbOnInitReq $ \onInitReq -> do
    fork "pushing ondc on_init logs" $
      void $ pushLogs "on_init" (toJSON onInitReq) merchantId.getId "MOBILITY"
    processOnInit onInitReq

-- | Direct confirm. Callbacks flow through modified CallBAP (DirectBAPCallback).
directConfirm ::
  R.FlowRuntime ->
  ConfirmAPI.ConfirmReqV2 ->
  Id Merchant.Merchant ->
  Flow ()
directConfirm bppFlowRt reqV2 merchantId = do
  fork "pushing ondc confirm logs" $
    void $ pushLogs "confirm" (toJSON reqV2) merchantId.getId "MOBILITY"

  bppMerchantId <- getBppMerchantId reqV2.confirmReqContext.contextBppId
  withBPPFlow bppFlowRt $ BPPDirect.handleConfirm (Id bppMerchantId) reqV2

-- | Direct cancel. Returns on_cancel response and processes in BAP.
directCancel ::
  R.FlowRuntime ->
  Id Merchant.Merchant ->
  CancelAPI.CancelReqV2 ->
  Flow ()
directCancel bppFlowRt merchantId reqV2 = do
  fork "pushing ondc cancel logs" $
    void $ pushLogs "cancel" (toJSON reqV2) merchantId.getId "MOBILITY"

  bppMerchantId <- getBppMerchantId reqV2.cancelReqContext.contextBppId
  mbOnCancelReq <- withBPPFlow bppFlowRt $ BPPDirect.handleCancel (Id bppMerchantId) reqV2

  -- Push ONDC log and process on_cancel response in BAP
  whenJust mbOnCancelReq $ \onCancelReq -> do
    fork "pushing ondc on_cancel logs" $
      void $ pushLogs "on_cancel" (toJSON onCancelReq) merchantId.getId "MOBILITY"
    DirectBAPCallback.processOnCancel onCancelReq

-- | Direct status. Returns on_status response and processes in BAP.
directStatus ::
  R.FlowRuntime ->
  StatusAPI.StatusReqV2 ->
  Id Merchant.Merchant ->
  Flow ()
directStatus bppFlowRt reqV2 merchantId = do
  fork "pushing ondc status logs" $
    void $ pushLogs "status" (toJSON reqV2) merchantId.getId "MOBILITY"

  bppMerchantId <- getBppMerchantId reqV2.statusReqContext.contextBppId
  mbOnStatusReq <- withBPPFlow bppFlowRt $ BPPDirect.handleStatus (Id bppMerchantId) reqV2

  -- Push ONDC log and process on_status response in BAP
  whenJust mbOnStatusReq $ \onStatusReq -> do
    fork "pushing ondc on_status logs" $
      void $ pushLogs "on_status" (toJSON onStatusReq) merchantId.getId "MOBILITY"
    DirectBAPCallback.processOnStatus onStatusReq

-- | Direct update. Fire-and-forget.
-- Responses arrive asynchronously via CallBAP (DirectBAPCallback).
directUpdate ::
  R.FlowRuntime ->
  UpdateAPI.UpdateReqV2 ->
  Flow ()
directUpdate bppFlowRt reqV2 = do
  bppMerchantId <- getBppMerchantId reqV2.updateReqContext.contextBppId
  withBPPFlow bppFlowRt $ BPPDirect.handleUpdate (Id bppMerchantId) reqV2

-- | Direct track. For now, falls through to HTTP (not yet fully implemented).
directTrack ::
  R.FlowRuntime ->
  TrackAPI.TrackReqV2 ->
  Id Merchant.Merchant ->
  Flow ()
directTrack _bppFlowRt _reqV2 _merchantId = do
  -- Track uses Callback.withCallback which sends HTTP to BAP.
  -- For v1, track is low-priority and can fall through to HTTP path.
  -- The withDirectBPP at the call site will fall back to HTTP.
  throwError $ InternalError "directTrack: not yet implemented, use HTTP fallback"

-- | Direct rating. Fire-and-forget.
directRating ::
  R.FlowRuntime ->
  RatingAPI.RatingReqV2 ->
  Id Merchant.Merchant ->
  Flow ()
directRating bppFlowRt reqV2 merchantId = do
  fork "pushing ondc rating logs" $
    void $ pushLogs "rating" (toJSON reqV2) merchantId.getId "MOBILITY"

  bppMerchantId <- getBppMerchantId reqV2.ratingReqContext.contextBppId
  withBPPFlow bppFlowRt $ BPPDirect.handleRating (Id bppMerchantId) reqV2

-- | Process on_init response from BPP in BAP context.
-- Mirrors API.Beckn.OnInit.onInit handler.
-- Lives in DirectBPPCall (not DirectBAPCallback) to avoid circular dependency,
-- since it calls directConfirm/directCancel from this module.
processOnInit :: Spec.OnInitReq -> Flow ()
processOnInit reqV2 = do
  transactionId <- Utils.getTransactionId reqV2.onInitReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logInfo $ "Direct on_init processing:-" <> show reqV2
    mbDOnInitReq <- ACLOnInit.buildOnInitReqV2 reqV2
    if isJust mbDOnInitReq
      then do
        let onInitReq = fromJust mbDOnInitReq
        Redis.whenWithLockRedis (onInitLockKey onInitReq.bookingId.getId) 60 $
          fork "direct on_init request processing" $ do
            (onInitRes, booking) <- DOnInit.onInit onInitReq
            fork "direct on_init pushing ondc logs" do
              void $ pushLogs "on_init" (toJSON reqV2) onInitRes.merchant.id.getId "MOBILITY"
            handle (errHandler booking) $ do
              confirmBecknReq <- ACLConfirm.buildConfirmReqV2 onInitRes
              Metrics.startMetricsBap Metrics.CONFIRM onInitRes.merchant.name transactionId booking.merchantOperatingCityId.getId
              withDirectBPP
                (\rt -> directConfirm rt confirmBecknReq onInitRes.merchant.id)
                (void . withShortRetry $ CallBPP.confirmV2 onInitRes.bppUrl confirmBecknReq onInitRes.merchant.id)
      else do
        let cancellationReason = "on_init API failure"
            cancelReq = buildCancelReq cancellationReason OnInit
        booking <- QRB.findByTransactionId transactionId >>= fromMaybeM (BookingNotFound $ "transactionId:-" <> transactionId)
        errHandlerAction booking cancelReq
  where
    errHandler booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = do
          let cancellationReason = "Confirm Beckn API Call failure"
              cancelReq = buildCancelReq cancellationReason OnConfirm
          errHandlerAction booking cancelReq
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = do
          let cancellationReason = "External API Call failure, during confirm beckn api"
              cancelReq = buildCancelReq cancellationReason OnConfirm
          errHandlerAction booking cancelReq
      | otherwise = throwM exc

    errHandlerAction booking cancelReq = do
      dCancelRes <- DCancel.cancel booking Nothing cancelReq SBCR.ByApplication
      cancelBecknReq <- ACLCancel.buildCancelReqV2 dCancelRes Nothing
      withDirectBPP
        (\rt -> directCancel rt booking.merchantId cancelBecknReq)
        (void . withShortRetry $ CallBPP.cancelV2 booking.merchantId dCancelRes.bppUrl cancelBecknReq)

    buildCancelReq cancellationReason reasonStage =
      DCancel.CancelReq
        { reasonCode = CancellationReasonCode cancellationReason,
          reasonStage,
          additionalInfo = Nothing,
          reallocate = Nothing,
          blockOnCancellationRate = Nothing
        }

onInitLockKey :: Text -> Text
onInitLockKey id = "Customer:OnInit:BookingId-" <> id
