{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | One-shot assignment skips the Beckn on_select\/init\/on_init\/confirm\/on_confirm
-- relay (dev/docs/one-shot-assign-plan.md), but ONDC still expects both NPs to push
-- transaction logs for the full flow. This module synthesizes the five skipped
-- payloads with the same builders the legacy relay uses, pushes the BPP-side logs,
-- and hands the callback payloads to the BAP (internal\/oneShotOndcLogs) so it can
-- push its receive side; the response carries the BAP-built init\/confirm back so
-- the BPP can log those as received. Called only from the assignment fork AFTER the
-- assignment callback succeeded — nothing here is on any critical path, and a
-- failure loses logs, never a ride.
module SharedLogic.OneShotOndcLogs (pushOneShotOndcLogs) where

import qualified Beckn.ACL.OnInit as ACLOnInit
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as UtilsV2
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Domain.Action.Beckn.Init as DInit
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequestForDriver as DSRFD
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import qualified SharedLogic.FarePolicy as SFP
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error
import TransactionLogs.PushLogs

pushOneShotOndcLogs ::
  DM.Merchant ->
  DSR.SearchRequest ->
  DSRFD.SearchRequestForDriver ->
  DST.SearchTry ->
  DDQ.DriverQuote ->
  DRB.Booking ->
  DRide.Ride ->
  DPerson.Person ->
  DVeh.Vehicle ->
  Flow ()
pushOneShotOndcLogs merchant searchReq srfd searchTry driverQuote booking ride driver vehicle = do
  now <- getCurrentTime
  -- Flow-ordered synthetic timestamps: the payloads are built out of order on two
  -- services, but the logged chain must advance in time like the real relay would.
  let tOnSelect = now
      tInit = addUTCTime 1 now
      tOnInit = addUTCTime 2 now
      tConfirm = addUTCTime 3 now
      tOnConfirm = addUTCTime 4 now
  -- on_init/on_confirm must carry the same message_id as the init/confirm they
  -- answer; both are minted here and the BAP patches its payloads to match.
  initMsgId <- generateGUID
  confirmMsgId <- generateGUID
  onSelectReq <- patchContext Nothing tOnSelect <$> buildDriverOfferOnSelect
  onInitReq <- buildSyntheticOnInit initMsgId tOnInit
  onConfirmReq <- buildSyntheticOnConfirm confirmMsgId tOnConfirm
  -- Same requestType strings the legacy paths use for these messages (send wrapper:
  -- show Context.ON_SELECT/ON_CONFIRM; withCallback: "on_init"; receive: "init"/"confirm").
  void $ pushLogs (show Context.ON_SELECT) (toJSON onSelectReq) merchant.id.getId "MOBILITY"
  void $ pushLogs "on_init" (toJSON onInitReq) merchant.id.getId "MOBILITY"
  void $ pushLogs (show Context.ON_CONFIRM) (toJSON onConfirmReq) merchant.id.getId "MOBILITY"
  appBackendBapInternal <- asks (.appBackendBapInternal)
  res <-
    CallBAPInternal.oneShotOndcLogs appBackendBapInternal.apiKey appBackendBapInternal.url $
      CallBAPInternal.OneShotOndcLogsReq
        { transactionId = searchReq.transactionId,
          bppBookingId = booking.id.getId,
          initMessageId = initMsgId,
          confirmMessageId = confirmMsgId,
          initTimestamp = tInit,
          confirmTimestamp = tConfirm,
          onSelectPayload = toJSON onSelectReq,
          onInitPayload = toJSON onInitReq,
          onConfirmPayload = toJSON onConfirmReq
        }
  whenJust res.initPayload $ \payload -> void $ pushLogs "init" payload merchant.id.getId "MOBILITY"
  whenJust res.confirmPayload $ \payload -> void $ pushLogs "confirm" payload merchant.id.getId "MOBILITY"
  where
    patchContext mbMsgId ts req = req {Spec.onSelectReqContext = ContextV2.setContextMessageIdAndTimestamp mbMsgId ts req.onSelectReqContext}

    buildDriverOfferOnSelect = BP.buildDriverOfferPayload merchant searchReq srfd searchTry driverQuote

    findBecknConfig =
      QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier)
        >>= fromMaybeM (InternalError "Beckn Config not found")

    buildContext action msgId ts mbTtlSec = do
      bppUri <- BP.buildBppUrl merchant.id
      ttl <- mbTtlSec & fromMaybeM (InternalError "Invalid ttl") <&> UtilsV2.computeTtlISO8601
      context <- ContextV2.buildContextV2 action Context.MOBILITY msgId (Just searchReq.transactionId) searchReq.bapId searchReq.bapUri (Just (getShortId merchant.subscriberId)) (Just bppUri) (fromMaybe merchant.city searchReq.bapCity) (fromMaybe Context.India searchReq.bapCountry) (Just ttl)
      pure $ ContextV2.setContextMessageIdAndTimestamp Nothing ts context

    buildSyntheticOnInit msgId ts = do
      bppConfig <- findBecknConfig
      isValueAddNP <- CQVAN.isValueAddNP searchReq.bapId
      mbFarePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback booking.quoteId
      riderId <- searchReq.riderId & fromMaybeM (InternalError "riderId missing on one-shot search request")
      riderDetails <- QRD.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
      riderPhoneNumber <- decrypt riderDetails.mobileNumber
      -- Same fresh GUID the legacy init handler mints for the on_init payment id.
      paymentId <- generateGUID
      let initRes =
            DInit.InitRes
              { booking = booking,
                transporter = merchant,
                paymentMethodInfo = Nothing,
                driverName = Just driver.firstName,
                driverId = Just driver.id.getId,
                bppSubscriberId = Just (getShortId merchant.subscriberId),
                riderPhoneNumber = riderPhoneNumber,
                riderName = booking.riderName,
                vehicleVariant = vehicle.variant,
                paymentId = paymentId,
                cancellationFee = Nothing,
                estimateId = driverQuote.estimateId.getId,
                riderGender = Nothing
              }
          onInitMsg = ACLOnInit.mkOnInitMessageV2 isValueAddNP initRes bppConfig mbFarePolicy
      context <- buildContext Context.ON_INIT msgId ts bppConfig.onInitTTLSec
      pure $ Spec.OnInitReq {onInitReqContext = context, onInitReqError = Nothing, onInitReqMessage = Just onInitMsg}

    buildSyntheticOnConfirm msgId ts = do
      bppConfig <- findBecknConfig
      onConfirmMsg <- BP.buildOnConfirmMessage booking ride driver vehicle
      context <- buildContext Context.ON_CONFIRM msgId ts bppConfig.onConfirmTTLSec
      pure $ Spec.OnConfirmReq {onConfirmReqContext = context, onConfirmReqError = Nothing, onConfirmReqMessage = Just onConfirmMsg}
