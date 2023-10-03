{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.ConfirmBus
  ( API,
    handler,
    confirmBus,
    ConfirmBusRes (..),
  )
where

import qualified Beckn.ACL.Init as ACL
import qualified Domain.Action.UI.ConfirmBus as DConfirmBus
-- import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as Quote
import qualified Domain.Types.Ticket as DTT
import Environment
import Kernel.Prelude hiding (init)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import Tools.Auth

type API =
  "rideSearch"
    :> TokenAuth
    :> "quotes"
    :> Capture "quoteId" (Id Quote.Quote)
    :> QueryParam "quantity" Integer
    :> "confirmbus"
    :> QueryParam "paymentMethodId" (Id DMPM.MerchantPaymentMethod)
    :> Post '[JSON] ConfirmBusRes

newtype ConfirmBusRes = ConfirmBusRes
  { ticketId :: Id DTT.Ticket
  }
  deriving (Show, FromJSON, ToJSON, Generic, ToSchema)

-------- Confirm Flow --------

handler :: FlowServer API
handler =
  confirmBus

-- It is confirm UI EP, but we call init beckn EP inside it. confirm beckn EP will be called in on_init
confirmBus ::
  (Id SP.Person, Id Merchant.Merchant) ->
  Id Quote.Quote ->
  Maybe Integer ->
  Maybe (Id DMPM.MerchantPaymentMethod) ->
  FlowHandler ConfirmBusRes
confirmBus (personId, _) quoteId quantity mbPaymentMethodId =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    dConfirmBusRes <- DConfirmBus.confirmBus personId quoteId quantity mbPaymentMethodId
    becknInitReq <- ACL.buildInitBusReq dConfirmBusRes
    handle (errHandler dConfirmBusRes.ticket) $
      void $ withShortRetry $ CallBPP.init dConfirmBusRes.providerUrl becknInitReq
    return $
      ConfirmBusRes
        { ticketId = dConfirmBusRes.ticket.id
        }
  where
    errHandler ticket exc
      | Just BecknAPICallError {} <- fromException @BecknAPICallError exc = DConfirmBus.cancelTicket ticket
      | Just ExternalAPICallError {} <- fromException @ExternalAPICallError exc = DConfirmBus.cancelTicket ticket
      | otherwise = throwM exc
