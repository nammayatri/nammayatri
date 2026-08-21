{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Confirm (API, handler) where

import qualified API.UI.Ride as RAPI
import qualified Beckn.ACL.Confirm as ACL
import qualified Beckn.ACL.OnConfirm as ACL
import qualified Beckn.OnDemand.Transformer.MSIL.OnConfirm as MSILOnConfirm
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.OnDemand.Utils.MSIL.Common as MSILTerms
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Merchant as DM
import Environment
import qualified EulerHS.Language as L
import Kernel.Beam.Types (TxnIdKey (..))
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Kernel.Utils.Servant.SignatureAuth
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Types.SpecialLocation as SL
import Servant hiding (throwError)
import qualified SharedLogic.Booking as SBooking
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.FarePolicy as SFP
import qualified SharedLogic.Ride as SRide
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BapMetadata as CQBapMetaData
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Tools.ActorInfo as ActorInfo
import Tools.Error
import TransactionLogs.PushLogs

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth 'Domain.MOBILITY "Authorization"
    :> Confirm.ConfirmAPIV2

handler :: FlowServer API
handler = confirm

confirm ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  Confirm.ConfirmReqV2 ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult _ subscriber) reqV2 = withFlowHandlerBecknAPI . ActorInfo.withRequestIdActorInfo $ do
  transactionId <- Utils.getTransactionId reqV2.confirmReqContext
  L.setOptionLocal TxnIdKey transactionId
  Utils.withTransactionIdLogTag transactionId $ do
    logTagInfo "Confirm APIV2 Flow" "Reached"
    let context = reqV2.confirmReqContext
        bppId = context.contextBppId
        txnId = Just transactionId
    bapId <- Utils.getContextBapId context
    callbackUrl <- Utils.getContextBapUri context
    bppUri <- Utils.getContextBppUri context
    msgId <- Utils.getMessageId context
    city <- Utils.getContextCity context
    country <- Utils.getContextCountry context
    isValueAddNP <- CQVAN.isValueAddNP bapId
    dConfirmReq <- ACL.buildConfirmReqV2 reqV2 isValueAddNP
    Redis.whenWithLockRedis (SRide.confirmLockKey dConfirmReq.bookingId) 60 $ do
      now <- getCurrentTime
      (transporter, eitherQuote) <- DConfirm.validateRequest subscriber transporterId dConfirmReq now
      -- Verifying: store the BAP's declared BAP_TERMS.STATIC_TERMS (if any)
      -- against its BapMetadata row, so it's available to echo back on this
      -- same on_confirm response. Pilot-gated, update-only -- see
      -- MSIL.Terms/CachedQueries.BapMetadata.
      moc <- CQMOC.findByMerchantIdAndCity transporterId city >>= fromMaybeM (InvalidRequest $ "Operating City " <> show city <> " not supported or not found")
      transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = moc.id.getId}) Nothing >>= fromMaybeM (TransporterConfigDoesNotExist moc.id.getId)
      let isMsilPilotMerchant = fromMaybe False transporterConfig.enableScheduledCategorySignal
      when isMsilPilotMerchant $ do
        let incomingOrderTags = reqV2.confirmReqMessage.confirmReqMessageOrder.orderTags
        MSILTerms.verifyIncomingStaticTerms (Id bapId) Domain.MOBILITY incomingOrderTags
      fork "confirm" $ do
        Redis.whenWithLockRedis (confirmProcessingLockKey dConfirmReq.bookingId.getId) 60 $ do
          dConfirmRes <- DConfirm.handler transporter dConfirmReq eitherQuote
          fork "confirm received pushing ondc logs" do
            void $ pushLogs "confirm" (toJSON reqV2) dConfirmRes.transporter.id.getId "MOBILITY"
          case dConfirmRes.rideInfo of
            Just rideInfo' -> do
              fork "on_confirm with rideInfo" $ do
                handle (errHandler dConfirmRes transporter (Just rideInfo'.driver)) $ do
                  void $ BP.sendOnConfirmToBAP dConfirmRes.booking rideInfo'.ride rideInfo'.driver rideInfo'.vehicle transporter context
                  when (isMeterRide dConfirmRes.booking.tripCategory) $ do
                    let startRideReq =
                          RAPI.StartRideReq
                            { rideOtp = "", -- doesn't matter for meter ride, not sure why this is not made Maybe, but not changing now as its not in scope of this PR. will do seperately later.
                              point = LatLong {lat = dConfirmRes.fromLocation.lat, lon = dConfirmRes.fromLocation.lon},
                              odometer = Nothing
                            }
                    void $ RAPI.startRide' (rideInfo'.driver.id, transporter.id, dConfirmRes.booking.merchantOperatingCityId) rideInfo'.ride.id startRideReq
            Nothing -> do
              fork "on_confirm on-us" $ do
                handle (errHandler dConfirmRes transporter Nothing) $ do
                  callOnConfirm dConfirmRes msgId txnId bapId callbackUrl bppId bppUri city country
    pure Ack
  where
    isMeterRide = \case
      DTC.OneWay DTC.MeterRide -> True
      _ -> False

    errHandler dConfirmRes transporter mbDriver exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = SBooking.cancelBooking dConfirmRes.booking mbDriver transporter
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = SBooking.cancelBooking dConfirmRes.booking mbDriver transporter
      | otherwise = throwM exc

    callOnConfirm dConfirmRes msgId txnId bapId callbackUrl bppId bppUri city country = do
      context <- ContextV2.buildContextV2 Context.CONFIRM Context.MOBILITY msgId txnId bapId callbackUrl bppId bppUri city country (Just "PT2M")
      let vehicleCategory = Utils.mapServiceTierToCategory dConfirmRes.booking.vehicleServiceTier
      becknConfig <- QBC.findByMerchantIdDomainAndVehicle dConfirmRes.transporter.id (show Context.MOBILITY) vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
      mbFarePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback dConfirmRes.booking.quoteId
      vehicleServiceTierItem <- CQVST.findByServiceTierTypeAndCityIdInRideFlow dConfirmRes.booking.vehicleServiceTier dConfirmRes.booking.merchantOperatingCityId (dConfirmRes.booking.area >>= SL.pickupSpecialZoneIdFromArea) >>= fromMaybeM (VehicleServiceTierNotFound (show dConfirmRes.booking.vehicleServiceTier))
      let pricing = Utils.convertBookingToPricing vehicleServiceTierItem dConfirmRes.booking
      bppInvoiceInfo <- ACL.resolveBPPInvoiceInfo dConfirmRes
      let onConfirmMessage' = ACL.buildOnConfirmMessageV2 dConfirmRes pricing becknConfig mbFarePolicy bppInvoiceInfo
      -- Pilot: cities with enableScheduledCategorySignal get both the NEW ->
      -- RIDE_CONFIRMED fulfillment-state fix and the BAP_TERMS/BPP_TERMS order-tag
      -- patch (echoing back the BAP's own declared STATIC_TERMS, if known, plus
      -- ours from becknConfig), applied in one pass by
      -- Beckn.OnDemand.Transformer.MSIL.OnConfirm.msilOnConfirmMessageBuild --
      -- unlike on_search/on_select/on_init, which only get BPP_TERMS. Everyone
      -- else's onConfirmMessage is exactly what Layer 1 built, unchanged.
      transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = dConfirmRes.booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigDoesNotExist dConfirmRes.booking.merchantOperatingCityId.getId)
      let isMsilPilotMerchant = fromMaybe False transporterConfig.enableScheduledCategorySignal
      onConfirmMessage <-
        if isMsilPilotMerchant
          then do
            mbBapMetadata <- CQBapMetaData.findBySubscriberIdAndDomain (Id bapId) Domain.MOBILITY
            let mbBapStaticTermsUrl = mbBapMetadata >>= (.staticTermsUrl)
            MSILOnConfirm.msilOnConfirmMessageBuild dConfirmRes.booking.isScheduled dConfirmRes.booking.transactionId mbBapStaticTermsUrl becknConfig onConfirmMessage'
          else pure onConfirmMessage'
      void $ BP.callOnConfirmV2 dConfirmRes.transporter context onConfirmMessage becknConfig

confirmProcessingLockKey :: Text -> Text
confirmProcessingLockKey id = "Driver:Confirm:Processing:BookingId-" <> id
