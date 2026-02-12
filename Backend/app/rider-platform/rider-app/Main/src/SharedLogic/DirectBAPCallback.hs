{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE PackageImports #-}

module SharedLogic.DirectBAPCallback
  ( mkDirectBAPCallback,
    processOnCancel,
    processOnStatus,
  )
where

import qualified Beckn.ACL.OnCancel as ACLOnCancel
import qualified Beckn.ACL.OnConfirm as ACLOnConfirm
import qualified Beckn.ACL.OnSearch as ACLOnSearch
import qualified Beckn.ACL.OnSelect as ACLOnSelect
import qualified Beckn.ACL.OnStatus as ACLOnStatus
import qualified Beckn.ACL.OnUpdate as ACLOnUpdate
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnCancel as DOnCancel
import qualified Domain.Action.Beckn.OnConfirm as DOnConfirm
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import Domain.Types (GatewayAndRegistryService (..), TripCategory (..))
import Domain.Types.Booking
import "dynamic-offer-driver-app" Environment (DirectBAPCallback (..))
import Environment
import qualified EulerHS.Runtime as R
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Flow (runFlowR)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.SearchRequest as QSearchReq
import Tools.Error
import qualified Tools.Metrics as Metrics
import TransactionLogs.PushLogs

-- | Create a DirectBAPCallback that processes BPP callbacks directly
-- in the BAP's FlowR context, bypassing HTTP.
-- Used for valueAddedNP co-located deployments.
mkDirectBAPCallback :: AppEnv -> R.FlowRuntime -> DirectBAPCallback
mkDirectBAPCallback bapEnv bapFlowRt =
  DirectBAPCallback
    { onSearchCallback = runInBAP handleOnSearch,
      onSelectCallback = runInBAP handleOnSelect,
      onConfirmCallback = runInBAP handleOnConfirm,
      onStatusCallback = runInBAP handleOnStatus,
      onUpdateCallback = runInBAP handleOnUpdate,
      onCancelCallback = runInBAP handleOnCancel
    }
  where
    runInBAP :: (a -> Flow ()) -> a -> IO ()
    runInBAP handler req = runFlowR bapFlowRt bapEnv (handler req)

-- | Handle on_search callback from BPP.
-- Mirrors API.Beckn.OnSearch.onSearch
handleOnSearch :: Spec.OnSearchReq -> Flow ()
handleOnSearch reqV2 = do
  transactionId <- Utils.getTransactionId reqV2.onSearchReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logInfo $ "Direct on_search received:-" <> show reqV2
    searchRequest <- runInReplica $ QSearchReq.findById (cast $ Id transactionId) >>= fromMaybeM (SearchRequestDoesNotExist transactionId)
    mbDOnSearchReq <- ACLOnSearch.buildOnSearchReqV2 reqV2
    messageId <- Utils.getMessageIdText reqV2.onSearchReqContext
    whenJust mbDOnSearchReq $ \request -> do
      let bppSubId = request.providerInfo.providerId
      Redis.whenWithLockRedis (onSearchLockKey messageId bppSubId) 60 $ do
        validatedRequest <- DOnSearch.validateRequest request searchRequest
        fork "direct on_search pushing ondc logs" do
          void $ pushLogs "on_search" (toJSON reqV2) validatedRequest.merchant.id.getId "MOBILITY"
        fork "direct on_search processing" $ do
          Redis.whenWithLockRedis (onSearchProcessingLockKey messageId bppSubId) 60 $
            DOnSearch.onSearch transactionId validatedRequest

-- | Handle on_select callback from BPP.
-- Mirrors API.Beckn.OnSelect.onSelect
handleOnSelect :: Spec.OnSelectReq -> Flow ()
handleOnSelect reqV2 = do
  transactionId <- Utils.getTransactionId reqV2.onSelectReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logInfo $ "Direct on_select received:-" <> show reqV2
    mbDOnSelectReq <- ACLOnSelect.buildOnSelectReqV2 reqV2
    messageId <- Utils.getMessageIdText reqV2.onSelectReqContext
    whenJust mbDOnSelectReq $ \onSelectReq ->
      Redis.whenWithLockRedis (onSelectLockKey messageId) 60 $ do
        validatedOnSelectReq <- DOnSelect.validateRequest onSelectReq
        fork "direct on_select pushing ondc logs" do
          void $ pushLogs "on_select" (toJSON reqV2) validatedOnSelectReq.searchRequest.merchantId.getId "MOBILITY"
        fork "direct on_select processing" $ do
          Redis.whenWithLockRedis (onSelectProcessingLockKey messageId) 60 $
            DOnSelect.onSelect validatedOnSelectReq

-- | Handle on_confirm callback from BPP.
-- Mirrors API.Beckn.OnConfirm.onConfirm
-- Note: isValueAddNP is always True in the direct flow.
handleOnConfirm :: Spec.OnConfirmReq -> Flow ()
handleOnConfirm reqV2 = do
  transactionId <- Utils.getTransactionId reqV2.onConfirmReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logInfo $ "Direct on_confirm received:-" <> show reqV2
    let isValueAddNP = True -- Always true in direct call flow
    mbDOnConfirmReq <- ACLOnConfirm.buildOnConfirmReqV2 reqV2 isValueAddNP
    whenJust mbDOnConfirmReq $ \onConfirmReq -> do
      let bppBookingId = case onConfirmReq of
            DOnConfirm.RideAssigned rideAssignedReq -> rideAssignedReq.bppBookingId
            DOnConfirm.BookingConfirmed bookingConfirmedReq -> bookingConfirmedReq.bppBookingId
      Redis.whenWithLockRedis (onConfirmLockKey bppBookingId.getId) 60 $ do
        validatedReq <- DOnConfirm.validateRequest onConfirmReq transactionId isValueAddNP
        merchantOperatingCityId <- case validatedReq of
          DOnConfirm.ValidatedRideAssigned rideAssignedReq -> pure $ rideAssignedReq.booking.merchantOperatingCityId.getId
          DOnConfirm.ValidatedBookingConfirmed bookingConfirmedReq -> pure $ bookingConfirmedReq.booking.merchantOperatingCityId.getId
        Metrics.finishMetricsBap Metrics.CONFIRM "" transactionId merchantOperatingCityId
        fork "direct on_confirm pushing ondc logs" do
          booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
          void $ pushLogs "on_confirm" (toJSON reqV2) booking.merchantId.getId "MOBILITY"
        runInForkWithCheck
          "direct on_confirm processing"
          ( pure $ case validatedReq of
              DOnConfirm.ValidatedRideAssigned rideAssignedReq -> rideAssignedReq.booking.tripCategory /= Just (OneWay MeterRide)
              _ -> True
          )
          (Redis.whenWithLockRedis (onConfirmProcessingLockKey bppBookingId.getId) 60 $ DOnConfirm.onConfirm validatedReq)
  where
    runInForkWithCheck message checkShouldFork action = do
      shouldFork <- checkShouldFork
      if shouldFork
        then fork message action
        else action

-- | Handle on_status callback from BPP.
-- Mirrors API.Beckn.OnStatus.onStatus
handleOnStatus :: Spec.OnStatusReq -> Flow ()
handleOnStatus reqV2 = do
  transactionId <- Utils.getTransactionId reqV2.onStatusReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logInfo $ "Direct on_status received:-" <> show reqV2
    messageId <- Utils.getMessageIdText reqV2.onStatusReqContext
    mbDOnStatusReq <- ACLOnStatus.buildOnStatusReqV2 reqV2 transactionId
    whenJust mbDOnStatusReq $ \onStatusReq ->
      Redis.whenWithLockRedis (onStatusLockKey messageId) 60 $ do
        validatedOnStatusReq <- DOnStatus.validateRequest onStatusReq
        fork "direct on_status processing" $ do
          Redis.whenWithLockRedis (onStatusProcessingLockKey messageId) 60 $
            DOnStatus.onStatus validatedOnStatusReq
          fork "direct on_status pushing ondc logs" do
            booking <- QRB.findByBPPBookingId onStatusReq.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> onStatusReq.bppBookingId.getId)
            void $ pushLogs "on_status" (toJSON reqV2) booking.merchantId.getId "MOBILITY"

-- | Handle on_update callback from BPP.
-- Mirrors API.Beckn.OnUpdate.onUpdate
handleOnUpdate :: Spec.OnUpdateReq -> Flow ()
handleOnUpdate reqV2 = do
  transactionId <- Utils.getTransactionId reqV2.onUpdateReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logInfo $ "Direct on_update received:-" <> show reqV2
    mbDOnUpdateReq <- ACLOnUpdate.buildOnUpdateReqV2 reqV2
    messageId <- Utils.getMessageIdText reqV2.onUpdateReqContext
    whenJust mbDOnUpdateReq $ \onUpdateReq ->
      Redis.whenWithLockRedis (onUpdateLockKey messageId) 60 $ do
        validatedOnUpdateReq <- DOnUpdate.validateRequest onUpdateReq
        fork "direct on_update processing" $ do
          Redis.whenWithLockRedis (onUpdateProcessingLockKey messageId) 60 $
            DOnUpdate.onUpdate validatedOnUpdateReq
        fork "direct on_update pushing ondc logs" do
          booking <- case validatedOnUpdateReq of
            DOnUpdate.OUValidatedScheduledRideAssignedReq req -> QRB.findByBPPBookingId req.bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> req.bookingDetails.bppBookingId.getId)
            DOnUpdate.OUValidatedRideAssignedReq req -> QRB.findByBPPBookingId req.bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> req.bookingDetails.bppBookingId.getId)
            DOnUpdate.OUValidatedRideStartedReq req -> QRB.findByBPPBookingId req.bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> req.bookingDetails.bppBookingId.getId)
            DOnUpdate.OUValidatedRideCompletedReq req -> QRB.findByBPPBookingId req.bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> req.bookingDetails.bppBookingId.getId)
            DOnUpdate.OUValidatedBookingCancelledReq req -> QRB.findByBPPBookingId req.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> req.bppBookingId.getId)
            DOnUpdate.OUValidatedBookingReallocationReq req -> return req.booking
            DOnUpdate.OUValidatedDriverArrivedReq req -> QRB.findByBPPBookingId req.bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> req.bookingDetails.bppBookingId.getId)
            DOnUpdate.OUValidatedEstimateRepetitionReq req -> return req.booking
            DOnUpdate.OUValidatedQuoteRepetitionReq req -> return req.booking
            DOnUpdate.OUValidatedNewMessageReq req -> return req.booking
            DOnUpdate.OUValidatedSafetyAlertReq req -> return req.booking
            DOnUpdate.OUValidatedPhoneCallRequestEventReq req -> return req.booking
            DOnUpdate.OUValidatedPhoneCallCompletedEventReq req -> return req.booking
            DOnUpdate.OUValidatedStopArrivedReq req -> return req.booking
            DOnUpdate.OUValidatedFarePaidReq req -> return req.booking
            DOnUpdate.OUValidatedEditDestSoftUpdateReq req -> return req.booking
            DOnUpdate.OUValidatedEditDestConfirmUpdateReq req -> return req.booking
            DOnUpdate.OUValidatedTollCrossedEventReq req -> return req.booking
            DOnUpdate.OUValidatedDestinationReachedReq req -> return req.booking
            DOnUpdate.OUValidatedEstimatedEndTimeRangeReq req -> return req.booking
            DOnUpdate.OUValidatedParcelImageFileUploadReq req -> return req.booking
            DOnUpdate.OUValidatedEditDestError _ -> throwError $ InternalError "Error request is not supported for network observability"
          void $ pushLogs "on_update" (toJSON reqV2) booking.merchantId.getId "MOBILITY"

-- | Handle on_cancel callback from BPP.
-- Mirrors API.Beckn.OnCancel.onCancel
handleOnCancel :: Spec.OnCancelReq -> Flow ()
handleOnCancel req = do
  transactionId <- Utils.getTransactionId req.onCancelReqContext
  Utils.withTransactionIdLogTag transactionId $ do
    logInfo $ "Direct on_cancel received:-" <> show req
    cancelMsg <- req.onCancelReqMessage & fromMaybeM (InvalidBecknSchema "Missing message in on_cancel")
    cancelStatus' <- cancelMsg.confirmReqMessageOrder.orderStatus & fromMaybeM (InvalidBecknSchema "Missing order.status in on_cancel message")
    cancelStatus <- readMaybe (T.unpack cancelStatus') & fromMaybeM (InvalidBecknSchema $ "Invalid order.status:-" <> cancelStatus')
    case cancelStatus of
      Enums.CANCELLED -> processCancellationRequest DOnCancel.onCancel
      Enums.SOFT_CANCEL -> processCancellationRequest DOnCancel.onSoftCancel
      _ -> throwError . InvalidBecknSchema $ "on_cancel order.status expected:-CANCELLED|SOFT_CANCEL, received:-" <> cancelStatus'
  where
    processCancellationRequest domainOnCancelAction = do
      mbDOnCancelReq <- ACLOnCancel.buildOnCancelReq req
      messageId <- Utils.getMessageIdText req.onCancelReqContext
      whenJust mbDOnCancelReq $ \onCancelReq ->
        Redis.whenWithLockRedis (onCancelLockKey messageId) 60 $ do
          validatedOnCancelReq <- DOnCancel.validateRequest onCancelReq
          fork "direct on_cancel processing" $ do
            Redis.whenWithLockRedis (onCancelProcessingLockKey messageId) 60 $ do
              domainOnCancelAction validatedOnCancelReq
              fork "direct on_cancel pushing ondc logs" do
                void $ pushLogs "on_cancel" (toJSON req) validatedOnCancelReq.booking.merchantId.getId "MOBILITY"

-- | Process on_cancel response in BAP context. Alias for handleOnCancel.
processOnCancel :: Spec.OnCancelReq -> Flow ()
processOnCancel = handleOnCancel

-- | Process on_status response in BAP context. Alias for handleOnStatus.
processOnStatus :: Spec.OnStatusReq -> Flow ()
processOnStatus = handleOnStatus

-- Lock keys (same format as HTTP handlers for idempotency)
onSearchLockKey :: Text -> Text -> Text
onSearchLockKey msgId bppSubscriberId = "Customer:OnSearch:MessageId-" <> msgId <> "-bppSubscriberId-" <> bppSubscriberId

onSearchProcessingLockKey :: Text -> Text -> Text
onSearchProcessingLockKey msgId bppSubscriberId = "Customer:OnSearch:Processing:MessageId-" <> msgId <> "-bppSubscriberId-" <> bppSubscriberId

onSelectLockKey :: Text -> Text
onSelectLockKey id = "Customer:OnSelect:MessageId-" <> id

onSelectProcessingLockKey :: Text -> Text
onSelectProcessingLockKey id = "Customer:OnSelect:Processing:MessageId-" <> id

onConfirmLockKey :: Text -> Text
onConfirmLockKey id = "Customer:OnConfirm:BppBookingId-" <> id

onConfirmProcessingLockKey :: Text -> Text
onConfirmProcessingLockKey id = "Customer:OnConfirm:Processing:BppBookingId-" <> id

onStatusLockKey :: Text -> Text
onStatusLockKey id = "Customer:OnStatus:MessageId-" <> id

onStatusProcessingLockKey :: Text -> Text
onStatusProcessingLockKey id = "Customer:OnStatus:Processing:MessageId-" <> id

onUpdateLockKey :: Text -> Text
onUpdateLockKey id = "Customer:OnUpdate:MessageId-" <> id

onUpdateProcessingLockKey :: Text -> Text
onUpdateProcessingLockKey id = "Customer:OnUpdate:Processing:MessageId-" <> id

onCancelLockKey :: Text -> Text
onCancelLockKey id = "Customer:OnCancel:MessageId-" <> id

onCancelProcessingLockKey :: Text -> Text
onCancelProcessingLockKey id = "Customer:OnCancel:Processing:MessageId-" <> id
