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
    confirm,
    ConfirmRes (..),
  )
where

import qualified Beckn.ACL.Init as ACL
import qualified Domain.Action.UI.Confirm as DConfirm
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as Quote
import Environment
import Kernel.Prelude hiding (init)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "rideSearch"
    :> TokenAuth
    :> "quotes"
    :> Capture "quoteId" (Id Quote.Quote)
    :> "confirm"
    :> QueryParam "paymentMethodId" (Id DMPM.MerchantPaymentMethod)
    :> Post '[JSON] ConfirmRes

newtype ConfirmRes = ConfirmRes
  { bookingId :: Id DRB.Booking
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
  Maybe (Id DMPM.MerchantPaymentMethod) ->
  FlowHandler ConfirmRes
confirm (personId, _) quoteId mbPaymentMethodId =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    dConfirmRes <- DConfirm.confirm personId quoteId mbPaymentMethodId
    isBecknSpecVersion2 <- asks (.isBecknSpecVersion2)
    if isBecknSpecVersion2
      then do
        becknInitReq <- ACL.buildInitReqV2 dConfirmRes
        handle (errHandler dConfirmRes.booking) $
          void $ withShortRetry $ CallBPP.initV2 dConfirmRes.providerUrl becknInitReq
      else do
        becknInitReq <- ACL.buildInitReq dConfirmRes
        handle (errHandler dConfirmRes.booking) $
          void $ withShortRetry $ CallBPP.init dConfirmRes.providerUrl becknInitReq
    return $
      ConfirmRes
        { bookingId = dConfirmRes.booking.id
        }
  where
    errHandler booking exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DConfirm.cancelBooking booking
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DConfirm.cancelBooking booking
      | otherwise = throwM exc
