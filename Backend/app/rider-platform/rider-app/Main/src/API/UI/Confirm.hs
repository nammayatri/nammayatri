{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Confirm
  ( API,
    handler,
    confirm',
    ConfirmRes (..),
  )
where

import qualified Beckn.ACL.Cancel as CancelACL
import qualified Beckn.ACL.Init as ACL
import qualified Beckn.OnDemand.Utils.Common as BecknUtils
import qualified Domain.Action.Beckn.OnCancel as DOnCancel
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Confirm as DConfirm
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as Quote
import Environment
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude hiding (init)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Lib.ConfigPilot.Interface.Types (getConfig)
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.Confirm as Fields
import qualified SharedLogic.Reschedule as SReschedule
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.BecknConfig (BecknConfigDimensions (..))
import qualified Storage.Queries.BecknConfig as SQBC
import qualified Storage.Queries.QueriesExtra.BookingLite as QBookingLite
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Auth
import Tools.Error (RescheduleError (..))
import qualified Tools.Fields as Fields
import Tools.FlowHandling (withFlowHandlerAPIPersonId)
import qualified Tools.Metrics as Metrics

type API =
  "rideSearch"
    :> TokenAuth
    :> "quotes"
    :> Capture "quoteId" (Id Quote.Quote)
    :> "confirm"
    :> QueryParam "paymentMethodId" Payment.PaymentMethodId
    :> QueryParam "paymentInstrument" DMPM.PaymentInstrument
    :> QueryParam "isAdvancedBookingEnabled" Bool
    :> QueryParam "selectedOfferId" Text
    :> QueryParam "supportsBookingDeposit" Bool
    :> QueryParam "numberOfPassengers" Int
    :> QueryParam "numberOfCheckInBags" Int
    :> QueryParam "numberOfCabinBags" Int
    :> Post '[JSON] ConfirmRes

data ConfirmRes = ConfirmRes
  { bookingId :: Id DRB.Booking,
    confirmTtl :: Int,
    bookingDepositAmount :: Maybe HighPrecMoney,
    bookingFeePending :: Bool
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

-------- Confirm Flow --------

handler :: FlowServer API
handler =
  confirm

-- It is confirm UI EP, but we call init beckn EP inside it. confirm beckn EP will be called in on_init
confirm ::
  (Id SP.Person, Id Merchant.Merchant) ->
  Id Quote.Quote ->
  Maybe Payment.PaymentMethodId ->
  Maybe DMPM.PaymentInstrument ->
  Maybe Bool ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler ConfirmRes
confirm (personId, merchantId) quoteId mbPaymentMethodId mbPaymentInstrument isAdvanceBookingEnabled mbSelectedOfferId mbSupportsBookingDeposit mbPassengers mbCheckInBags mbCabinBags = withFlowHandlerAPIPersonId personId $ confirm' (personId, merchantId) quoteId Nothing mbPaymentMethodId mbPaymentInstrument isAdvanceBookingEnabled mbSelectedOfferId mbSupportsBookingDeposit mbPassengers mbCheckInBags mbCabinBags

confirm' ::
  (Id SP.Person, Id Merchant.Merchant) ->
  Id Quote.Quote ->
  Maybe Text ->
  Maybe Payment.PaymentMethodId ->
  Maybe DMPM.PaymentInstrument ->
  Maybe Bool ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  Flow ConfirmRes
confirm' (personId, merchantId) quoteId mbDashboardAgentId mbPaymentMethodId mbPaymentInstrument isAdvanceBookingEnabled mbSelectedOfferId mbSupportsBookingDeposit mbPassengers mbCheckInBags mbCabinBags =
  withPersonIdLogTag personId $ do
    ensureRescheduleWithoutCancellationFee personId merchantId quoteId
    -- BoothOnline (Paytm EDC) always requires payment before confirm; derived from paymentInstrument only
    let requiresPaymentBeforeConfirm = mbPaymentInstrument == Just DMPM.BoothOnline && mbPaymentMethodId == Just "PAYTM_EDC"
    dConfirmRes <- DConfirm.confirm personId quoteId mbDashboardAgentId mbPaymentMethodId mbPaymentInstrument isAdvanceBookingEnabled requiresPaymentBeforeConfirm mbSupportsBookingDeposit mbSelectedOfferId mbPassengers mbCheckInBags mbCabinBags
    becknInitReq <- ACL.buildInitReqV2 dConfirmRes
    moc <- CQMOC.findByMerchantIdAndCity dConfirmRes.merchant.id dConfirmRes.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> dConfirmRes.merchant.id.getId <> "-city-" <> show dConfirmRes.city)
    bapConfig <- (listToMaybe <$> getConfig (BecknConfigDimensions {merchantOperatingCityId = moc.id.getId, merchantId = dConfirmRes.merchant.id.getId, domain = Just "MOBILITY", vehicleCategory = Nothing, becknProtocol = Nothing}) (Just (SQBC.findByMerchantIdDomainandMerchantOperatingCityId (Just dConfirmRes.merchant.id) "MOBILITY" (Just moc.id)))) >>= fromMaybeM (InvalidRequest $ "BecknConfig not found for merchantId " <> show dConfirmRes.merchant.id.getId <> " merchantOperatingCityId " <> show moc.id.getId)
    initTtl <- bapConfig.initTTLSec & fromMaybeM (InternalError "Invalid ttl")
    confirmTtl <- bapConfig.confirmTTLSec & fromMaybeM (InternalError "Invalid ttl")
    confirmBufferTtl <- bapConfig.confirmBufferTTLSec & fromMaybeM (InternalError "Invalid ttl")
    let ttlInInt = initTtl + confirmTtl + confirmBufferTtl
    handle (errHandler dConfirmRes.booking) $ do
      Metrics.startMetricsBap Metrics.INIT dConfirmRes.merchant.name dConfirmRes.searchRequestId.getId dConfirmRes.booking.merchantOperatingCityId.getId
      void . withShortRetry $ CallBPP.initV2 dConfirmRes.providerUrl becknInitReq dConfirmRes.merchant.id

    return $
      ConfirmRes
        { bookingId = dConfirmRes.booking.id,
          confirmTtl = ttlInInt,
          bookingDepositAmount = dConfirmRes.booking.bookingDepositAmount,
          bookingFeePending = dConfirmRes.bookingFeePending
        }
  where
    errHandler booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DConfirm.cancelBooking booking
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DConfirm.cancelBooking booking
      | otherwise = throwM exc

-- | Rescheduling must not dodge a cancellation fee: when cancelling the old booking (it has a
-- driver) would carry one right now, the reschedule is refused. Asks the BPP the same way the
-- app does before showing its cancel popup (a soft cancel, which changes nothing).
ensureRescheduleWithoutCancellationFee :: Id SP.Person -> Id Merchant.Merchant -> Id Quote.Quote -> Flow ()
ensureRescheduleWithoutCancellationFee personId merchantId quoteId = do
  mbQuote <- QQuote.findById quoteId
  mbSearchRequest <- maybe (pure Nothing) (QSR.findById . (.requestId)) mbQuote
  mbOldBooking <- maybe (pure Nothing) SReschedule.findActiveRescheduleSource mbSearchRequest
  whenJust mbOldBooking $ \oldBooking -> do
    mbRide <- QRide.findActiveByRBId oldBooking.id
    when (isJust mbRide) $ do
      bookingLite <- QBookingLite.findByIdLite oldBooking.id >>= fromMaybeM (BookingDoesNotExist oldBooking.id.getId)
      dCancelRes <- DCancel.softCancel bookingLite (personId, merchantId)
      cancelBecknReq <- CancelACL.buildCancelReqV2 dCancelRes Nothing
      void $ withShortRetry $ CallBPP.cancelV2 merchantId dCancelRes.bppUrl cancelBecknReq
      messageId <- BecknUtils.getMessageIdText cancelBecknReq.cancelReqContext
      mbSoftCancelQuote <- DOnCancel.getSoftCancelQuote messageId
      when (maybe False (\fee -> fee.amount > 0) (mbSoftCancelQuote >>= (.cancellationFee))) $
        throwError RescheduleNotAllowedInWindow
