{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Direct handlers for in-process BPP calls from BAP.
-- These functions encapsulate BPP handler logic and return Beckn V2 responses
-- instead of sending HTTP callbacks. Used by BAP's DirectBPPCall module
-- for valueAddedNP co-located deployments.
module SharedLogic.DirectHandler
  ( mkSubscriberForDirectCall,
    handleSelect,
    handleInit,
    handleConfirm,
    handleCancel,
    handleStatus,
    handleUpdate,
    handleRating,
  )
where

import qualified Beckn.ACL.Cancel as CancelACL
import qualified Beckn.ACL.Confirm as ConfirmACL
import qualified Beckn.ACL.Init as InitACL
import qualified Beckn.ACL.OnCancel as OnCancelACL
import qualified Beckn.ACL.OnConfirm as OnConfirmACL
import qualified Beckn.ACL.OnInit as OnInitACL
import qualified Beckn.ACL.OnStatus as OnStatusACL
import qualified Beckn.ACL.Rating as RatingACL
import qualified Beckn.ACL.Select as SelectACL
import qualified Beckn.ACL.Status as StatusACL
import qualified Beckn.ACL.Update as UpdateACL
import qualified Beckn.OnDemand.Utils.Common as Utils
import Beckn.Types.Core.Taxi.API.Cancel as CancelAPI
import Beckn.Types.Core.Taxi.API.Confirm as ConfirmAPI
import qualified Beckn.Types.Core.Taxi.API.Init as InitAPI
import Beckn.Types.Core.Taxi.API.Rating as RatingAPI
import qualified Beckn.Types.Core.Taxi.API.Search as SearchAPI
import Beckn.Types.Core.Taxi.API.Select as SelectAPI
import Beckn.Types.Core.Taxi.API.Status as StatusAPI
import Beckn.Types.Core.Taxi.API.Update as UpdateAPI
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Aeson as A
import qualified Domain.Action.Beckn.Cancel as DCancel
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Action.Beckn.Rating as DRating
import qualified Domain.Action.Beckn.Select as DSelect
import qualified Domain.Action.Beckn.Status as DStatus
import qualified Domain.Action.Beckn.Update as DUpdate
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.OnCancel as OC
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Base64 (Base64 (..))
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Common (PriceAPIEntity)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import qualified SharedLogic.Booking as SBooking
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.FarePolicy as SFP
import qualified SharedLogic.Ride as SRide
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.SearchTry as QST
import Tools.Error

-- | Construct a minimal subscriber for BPP ACL validation in direct calls.
-- The ACL functions only use subscriber_id (for validation) and subscriber_url (for bapUri).
mkSubscriberForDirectCall :: MonadFlow m => Spec.Context -> m Subscriber.Subscriber
mkSubscriberForDirectCall context = do
  bapId <- fromMaybeM (InvalidRequest "BapId missing in context") context.contextBapId
  bapUri <- Utils.getContextBapUri context
  now <- getCurrentTime
  pure
    Subscriber.Subscriber
      { unique_key_id = "direct-call",
        subscriber_id = bapId,
        subscriber_url = bapUri,
        _type = Subscriber.BAP,
        domain = Domain.MOBILITY,
        city = [],
        country = Nothing,
        signing_public_key = Base64 "",
        encr_public_key = Nothing,
        valid_from = Nothing,
        valid_until = Nothing,
        status = Just Subscriber.SUBSCRIBED,
        created = now,
        updated = now
      }

-- | Handle select request. Fire-and-forget.
-- Driver offers arrive asynchronously via CallBAP (DirectBAPCallback).
handleSelect :: Id DM.Merchant -> SelectAPI.SelectReqV2 -> Flow ()
handleSelect transporterId reqV2 = do
  subscriber <- mkSubscriberForDirectCall reqV2.selectReqContext
  dSelectReq <- SelectACL.buildSelectReqV2 subscriber reqV2
  Redis.whenWithLockRedis (selectLockKey dSelectReq.messageId) 60 $ do
    (merchant, searchRequest, estimates) <- DSelect.validateRequest transporterId dSelectReq
    fork "direct select processing" $
      Redis.whenWithLockRedis (selectProcessingLockKey dSelectReq.messageId) 60 $
        DSelect.handler merchant dSelectReq searchRequest estimates

-- | Handle init request. Returns on_init Beckn V2 response for BAP processing.
handleInit :: Id DM.Merchant -> InitAPI.InitReqV2 -> Flow (Maybe Spec.OnInitReq)
handleInit transporterId reqV2 = do
  subscriber <- mkSubscriberForDirectCall reqV2.initReqContext
  let context = reqV2.initReqContext
  bapUri <- Utils.getContextBapUri context
  bapId <- Utils.getContextBapId context
  bppUri <- Utils.getContextBppUri context
  msgId <- Utils.getMessageId context
  city <- Utils.getContextCity context
  country <- Utils.getContextCountry context
  transactionId <- Utils.getTransactionId context

  isValueAddNP <- CQVAN.isValueAddNP bapId
  dInitReq <- InitACL.buildInitReqV2 subscriber reqV2 isValueAddNP

  let txnId = Just transactionId
      initFulfillmentId =
        case dInitReq.fulfillmentId of
          DInit.DriverQuoteId (Id fId) -> fId
          DInit.QuoteId (Id fId) -> fId

  Redis.whenWithLockRedis (initLockKey initFulfillmentId) 60 $ do
    mbProcessed :: Maybe Text <- Redis.withMasterRedis $ Redis.get (initProcessedKey initFulfillmentId)
    if isJust mbProcessed
      then return Nothing
      else do
        validatedRes <- DInit.validateRequest transporterId dInitReq
        dInitRes <- DInit.handler transporterId dInitReq validatedRes

        let vehicleCategory = Utils.mapServiceTierToCategory dInitRes.booking.vehicleServiceTier
        bppConfig <- QBC.findByMerchantIdDomainAndVehicle dInitRes.transporter.id (show Context.MOBILITY) vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
        mbFarePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback dInitRes.booking.quoteId
        let onInitMessage = OnInitACL.mkOnInitMessageV2 dInitRes bppConfig mbFarePolicy
        ttl <- bppConfig.onInitTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
        responseContext <- ContextV2.buildContextV2 Context.ON_INIT Context.MOBILITY msgId txnId bapId bapUri context.contextBppId bppUri city country (Just ttl)

        Redis.setExp (initProcessedKey initFulfillmentId) ("PROCESSED" :: Text) 300

        return $
          Just
            Spec.OnInitReq
              { onInitReqContext = responseContext,
                onInitReqError = Nothing,
                onInitReqMessage = Just onInitMessage
              }

-- | Handle confirm request. Callbacks flow through modified CallBAP (DirectBAPCallback).
-- No response returned - on_confirm is dispatched via CallBAP internally.
handleConfirm :: Id DM.Merchant -> ConfirmAPI.ConfirmReqV2 -> Flow ()
handleConfirm transporterId reqV2 = do
  subscriber <- mkSubscriberForDirectCall reqV2.confirmReqContext
  let context = reqV2.confirmReqContext
      bppId = context.contextBppId
  transactionId <- Utils.getTransactionId context
  let txnId = Just transactionId
  bapId <- Utils.getContextBapId context
  callbackUrl <- Utils.getContextBapUri context
  bppUri <- Utils.getContextBppUri context
  msgId <- Utils.getMessageId context
  city <- Utils.getContextCity context
  country <- Utils.getContextCountry context

  isValueAddNP <- CQVAN.isValueAddNP bapId
  dConfirmReq <- ConfirmACL.buildConfirmReqV2 reqV2 isValueAddNP

  Redis.whenWithLockRedis (SRide.confirmLockKey dConfirmReq.bookingId) 60 $ do
    now <- getCurrentTime
    (transporter, eitherQuote) <- DConfirm.validateRequest subscriber transporterId dConfirmReq now
    Redis.whenWithLockRedis (confirmProcessingLockKey dConfirmReq.bookingId.getId) 60 $ do
      dConfirmRes <- DConfirm.handler transporter dConfirmReq eitherQuote
      case dConfirmRes.rideInfo of
        Just rideInfo' ->
          handle (errHandler dConfirmRes.booking transporter (Just rideInfo'.driver)) $ do
            void $ BP.sendOnConfirmToBAP dConfirmRes.booking rideInfo'.ride rideInfo'.driver rideInfo'.vehicle transporter context
        Nothing ->
          handle (errHandler dConfirmRes.booking transporter Nothing) $
            callOnConfirm dConfirmRes msgId txnId bapId callbackUrl bppId bppUri city country
  where
    errHandler booking transporter mbDriver exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = SBooking.cancelBooking booking mbDriver transporter
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = SBooking.cancelBooking booking mbDriver transporter
      | otherwise = throwM exc

    callOnConfirm dConfirmRes msgId txnId bapId callbackUrl bppId bppUri city country = do
      callbackContext <- ContextV2.buildContextV2 Context.CONFIRM Context.MOBILITY msgId txnId bapId callbackUrl bppId bppUri city country (Just "PT2M")
      let vehicleCategory = Utils.mapServiceTierToCategory dConfirmRes.booking.vehicleServiceTier
      becknConfig <- QBC.findByMerchantIdDomainAndVehicle dConfirmRes.transporter.id (show Context.MOBILITY) vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
      mbFarePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback dConfirmRes.booking.quoteId
      vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityIdInRideFlow dConfirmRes.booking.vehicleServiceTier dConfirmRes.booking.merchantOperatingCityId dConfirmRes.booking.configInExperimentVersions >>= fromMaybeM (VehicleServiceTierNotFound (show dConfirmRes.booking.vehicleServiceTier))
      let pricing = Utils.convertBookingToPricing vehicleServiceTierItem dConfirmRes.booking
          onConfirmMessage = OnConfirmACL.buildOnConfirmMessageV2 dConfirmRes pricing becknConfig mbFarePolicy
      void $ BP.callOnConfirmV2 dConfirmRes.transporter callbackContext onConfirmMessage becknConfig

-- | Handle cancel request.
-- For CancelRide: returns on_cancel Beckn V2 response for BAP processing.
-- For CancelSearch: fire-and-forget, returns Nothing.
handleCancel :: Id DM.Merchant -> CancelAPI.CancelReqV2 -> Flow (Maybe Spec.OnCancelReq)
handleCancel transporterId reqV2 = do
  dCancelReq <- CancelACL.buildCancelReqV2 reqV2
  let context = reqV2.cancelReqContext
  city <- Utils.getContextCity context
  country <- Utils.getContextCountry context
  msgId <- Utils.getMessageId context
  case dCancelReq of
    DCancel.CancelRide cancelRideReq -> do
      -- validateCancelRequest ignores SignatureAuthResult (pattern matched as _)
      (validMerchant, booking) <- DCancel.validateCancelRequest transporterId (error "SignatureAuthResult: unused in direct call") cancelRideReq
      let cancelStatus = A.decode . A.encode =<< cancelRideReq.cancelStatus
      case cancelStatus of
        Just Enums.CONFIRM_CANCEL ->
          Redis.whenWithLockRedis (cancelLockKey cancelRideReq.bookingId.getId) 60 $ do
            mbActiveSearchTry <- QST.findActiveTryByQuoteId booking.quoteId
            (isReallocated, cancellationCharge) <- DCancel.cancel cancelRideReq validMerchant booking mbActiveSearchTry
            if isReallocated
              then return Nothing
              else do
                let onCancelBuildReq =
                      OC.DBookingCancelledReqV2
                        { booking = booking,
                          cancellationSource = SBCR.ByUser,
                          cancellationFee = cancellationCharge
                        }
                onCancelReq <- OnCancelACL.buildOnCancelMessageV2 validMerchant (Just city) (Just country) (show Enums.CANCELLED) (OC.BookingCancelledBuildReqV2 onCancelBuildReq) (Just msgId)
                return $ Just onCancelReq
        Just Enums.SOFT_CANCEL -> do
          let onCancelBuildReq =
                OC.DBookingCancelledReqV2
                  { booking = booking,
                    cancellationSource = SBCR.ByUser,
                    cancellationFee = Nothing
                  }
          onCancelReq <- OnCancelACL.buildOnCancelMessageV2 validMerchant (Just city) (Just country) (show Enums.SOFT_CANCEL) (OC.BookingCancelledBuildReqV2 onCancelBuildReq) (Just msgId)
          return $ Just onCancelReq
        _ -> throwError $ InvalidRequest "Invalid cancel status"
    DCancel.CancelSearch cancelSearchReq -> do
      -- validateCancelSearchRequest ignores SignatureAuthResult (pattern matched as _)
      searchTry <- DCancel.validateCancelSearchRequest transporterId (error "SignatureAuthResult: unused in direct call") cancelSearchReq
      DCancel.cancelSearch transporterId searchTry
      return Nothing

-- | Handle status request. Returns on_status Beckn V2 response for BAP processing.
handleStatus :: Id DM.Merchant -> StatusAPI.StatusReqV2 -> Flow (Maybe Spec.OnStatusReq)
handleStatus transporterId reqV2 = do
  subscriber <- mkSubscriberForDirectCall reqV2.statusReqContext
  dStatusReq <- StatusACL.buildStatusReqV2 subscriber reqV2
  dStatusRes <- DStatus.handler transporterId dStatusReq
  msgId <- Utils.getMessageId reqV2.statusReqContext
  onStatusReq <- OnStatusACL.buildOnStatusReqV2 dStatusRes.transporter dStatusRes.booking dStatusRes.info (Just msgId)
  return $ Just onStatusReq

-- | Handle update request. Fire-and-forget.
-- Responses arrive asynchronously via CallBAP (DirectBAPCallback).
handleUpdate :: Id DM.Merchant -> UpdateAPI.UpdateReqV2 -> Flow ()
handleUpdate transporterId reqV2 = do
  subscriber <- mkSubscriberForDirectCall reqV2.updateReqContext
  dUpdateReq <- UpdateACL.buildUpdateReq transporterId subscriber reqV2
  let bookingId = DUpdate.getBookingId dUpdateReq
  Redis.whenWithLockRedis (updateLockKey bookingId.getId) 60 $
    fork "direct update processing" $
      Redis.whenWithLockRedis (updateProcessingLockKey bookingId.getId) 60 $
        DUpdate.handler dUpdateReq

-- | Handle rating request. Fire-and-forget.
handleRating :: Id DM.Merchant -> RatingAPI.RatingReqV2 -> Flow ()
handleRating transporterId reqV2 = do
  subscriber <- mkSubscriberForDirectCall reqV2.ratingReqContext
  dRatingReq <- RatingACL.buildRatingReqV2 subscriber reqV2
  Redis.whenWithLockRedis (ratingLockKey dRatingReq.bookingId.getId) 60 $ do
    ride <- DRating.validateRequest dRatingReq
    fork "direct rating processing" $
      Redis.whenWithLockRedis (ratingProcessingLockKey dRatingReq.bookingId.getId) 60 $
        DRating.handler transporterId dRatingReq ride

-- Lock keys (matching BPP HTTP handler format for cross-path idempotency)
selectLockKey :: Text -> Text
selectLockKey id = "Driver:Select:MessageId-" <> id

selectProcessingLockKey :: Text -> Text
selectProcessingLockKey id = "Driver:Select:Processing:MessageId-" <> id

initLockKey :: Text -> Text
initLockKey id = "Driver:Init:DriverQuoteId-" <> id

initProcessingLockKey :: Text -> Text
initProcessingLockKey id = "Driver:Init:Processing:DriverQuoteId-" <> id

initProcessedKey :: Text -> Text
initProcessedKey id = "Driver:Init:Processed:FulfillmentId-" <> id

confirmProcessingLockKey :: Text -> Text
confirmProcessingLockKey id = "Driver:Confirm:Processing:BookingId-" <> id

cancelLockKey :: Text -> Text
cancelLockKey id = "Driver:Cancel:BookingId-" <> id

updateLockKey :: Text -> Text
updateLockKey id = "Driver:Update:BookingId-" <> id

updateProcessingLockKey :: Text -> Text
updateProcessingLockKey id = "Driver:Update:Processing:BookingId-" <> id

ratingLockKey :: Text -> Text
ratingLockKey id = "Driver:Rating:BookingId-" <> id

ratingProcessingLockKey :: Text -> Text
ratingProcessingLockKey id = "Driver:Rating:Processing:BookingId-" <> id
