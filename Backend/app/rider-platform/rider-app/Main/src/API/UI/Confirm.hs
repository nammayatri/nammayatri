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

import qualified Beckn.ACL.Init as ACL
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
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Auth
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
    :> Post '[JSON] ConfirmRes

data ConfirmRes = ConfirmRes
  { bookingId :: Id DRB.Booking,
    confirmTtl :: Int
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
  FlowHandler ConfirmRes
confirm (personId, merchantId) quoteId mbPaymentMethodId mbPaymentInstrument = withFlowHandlerAPI . confirm' (personId, merchantId) quoteId mbPaymentMethodId mbPaymentInstrument

confirm' ::
  (Id SP.Person, Id Merchant.Merchant) ->
  Id Quote.Quote ->
  Maybe Payment.PaymentMethodId ->
  Maybe DMPM.PaymentInstrument ->
  Maybe Bool ->
  Flow ConfirmRes
confirm' (personId, _) quoteId mbPaymentMethodId mbPaymentInstrument isAdvanceBookingEnabled =
  withPersonIdLogTag personId $ do
    dConfirmRes <- DConfirm.confirm personId quoteId mbPaymentMethodId mbPaymentInstrument isAdvanceBookingEnabled
    becknInitReq <- ACL.buildInitReqV2 dConfirmRes
    moc <- CQMOC.findByMerchantIdAndCity dConfirmRes.merchant.id dConfirmRes.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> dConfirmRes.merchant.id.getId <> "-city-" <> show dConfirmRes.city)
    bapConfigs <- QBC.findByMerchantIdDomainandMerchantOperatingCityId dConfirmRes.merchant.id "MOBILITY" moc.id
    bapConfig <- listToMaybe bapConfigs & fromMaybeM (InvalidRequest $ "BecknConfig not found for merchantId " <> show dConfirmRes.merchant.id.getId <> " merchantOperatingCityId " <> show moc.id.getId) -- Using findAll for backward compatibility, TODO : Remove findAll and use findOne
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
          confirmTtl = ttlInInt
        }
  where
    errHandler booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DConfirm.cancelBooking booking
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DConfirm.cancelBooking booking
      | otherwise = throwM exc
