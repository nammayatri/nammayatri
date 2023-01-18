module API.UI.Confirm
  ( API,
    handler,
    confirm,
    ConfirmRes (..),
  )
where

import Beckn.Prelude hiding (init)
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError
import qualified Core.ACL.Init as ACL
import qualified Domain.Action.UI.Confirm as DConfirm
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as Quote
import Environment
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import Tools.Auth

type API =
  "rideSearch"
    :> TokenAuth
    :> "quotes"
    :> Capture "quoteId" (Id Quote.Quote)
    :> "confirm"
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
  Id SP.Person ->
  Id Quote.Quote ->
  FlowHandler ConfirmRes
confirm personId quoteId =
  withFlowHandlerAPI . withPersonIdLogTag personId $ do
    dConfirmRes <- DConfirm.confirm personId quoteId
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
