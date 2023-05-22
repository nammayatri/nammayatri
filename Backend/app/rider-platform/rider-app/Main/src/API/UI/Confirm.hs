{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module API.UI.Confirm
  ( API,
    handler,
    confirm,
    ConfirmRes (..),
  )
where

import qualified Beckn.ACL.Init as ACL
import qualified Domain.Action.UI.Confirm as DConfirm
import Environment
import Kernel.Prelude hiding (init)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import Servant
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.SimulatedFlow.Confirm
import qualified SharedLogic.Types.Person as SP
import qualified SharedLogic.Types.Quote as Quote
import qualified Storage.Queries.Person as QP
import Tools.Auth

type API =
  "rideSearch"
    :> TokenAuth
    :> "quotes"
    :> Capture "quoteId" (Id Quote.Quote)
    :> "confirm"
    :> Post '[JSON] ConfirmRes

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
    person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    if person.isSimulated
      then do
        simulateBooking quoteId
      else do
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
