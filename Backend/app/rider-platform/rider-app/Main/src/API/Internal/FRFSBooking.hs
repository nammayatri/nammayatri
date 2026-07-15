{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.FRFSBooking where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import qualified Domain.Action.Internal.FRFSBooking as Domain
import qualified Domain.Types.FRFSQuote as DFRFSQuote
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Environment
import EulerHS.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()
import Tools.Error

-- Internal (driver-app / BPP → rider-app / BAP) FRFS booking endpoints used by
-- the conductor tablet. Mirrors the mobile search → quote → confirm → status
-- flow; @token@ is the internal API key.
type API =
  "frfs"
    :> "booking"
    :> ( "search"
           :> Header "token" Text
           :> ReqBody '[JSON] Domain.FRFSBookingSearchReq
           :> Post '[JSON] FRFSTicketServiceAPI.FRFSSearchAPIRes
           :<|> "search"
             :> Capture "searchId" (Id DFRFSSearch.FRFSSearch)
             :> "quote"
             :> Header "token" Text
             :> Get '[JSON] [FRFSTicketServiceAPI.FRFSQuoteAPIRes]
           :<|> "quote"
             :> Capture "quoteId" (Id DFRFSQuote.FRFSQuote)
             :> "confirm"
             :> Header "token" Text
             :> ReqBody '[JSON] FRFSTicketServiceAPI.FRFSQuoteConfirmReq
             :> Post '[JSON] FRFSTicketServiceAPI.FRFSTicketBookingStatusAPIRes
           :<|> Capture "bookingId" (Id DFRFSTicketBooking.FRFSTicketBooking)
             :> "status"
             :> Header "token" Text
             :> Get '[JSON] FRFSTicketServiceAPI.FRFSTicketBookingStatusAPIRes
       )

handler :: FlowServer API
handler =
  frfsBookingSearch
    :<|> frfsBookingQuote
    :<|> frfsBookingConfirm
    :<|> frfsBookingStatus

-- | Reject any internal FRFS call whose @token@ header does not match the rider-app
--   internal API key. Applied to every route below so these endpoints are never open.
verifyInternalToken :: Maybe Text -> Flow ()
verifyInternalToken mbToken = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $ throwError (AuthBlocked "Invalid BAP internal api key")

frfsBookingSearch :: Maybe Text -> Domain.FRFSBookingSearchReq -> FlowHandler FRFSTicketServiceAPI.FRFSSearchAPIRes
frfsBookingSearch token req = withFlowHandlerAPI $ verifyInternalToken token >> Domain.frfsBookingSearch req

frfsBookingQuote :: Id DFRFSSearch.FRFSSearch -> Maybe Text -> FlowHandler [FRFSTicketServiceAPI.FRFSQuoteAPIRes]
frfsBookingQuote searchId token = withFlowHandlerAPI $ verifyInternalToken token >> Domain.frfsBookingQuote searchId

frfsBookingConfirm :: Id DFRFSQuote.FRFSQuote -> Maybe Text -> FRFSTicketServiceAPI.FRFSQuoteConfirmReq -> FlowHandler FRFSTicketServiceAPI.FRFSTicketBookingStatusAPIRes
frfsBookingConfirm quoteId token req = withFlowHandlerAPI $ verifyInternalToken token >> Domain.frfsBookingConfirm quoteId req

frfsBookingStatus :: Id DFRFSTicketBooking.FRFSTicketBooking -> Maybe Text -> FlowHandler FRFSTicketServiceAPI.FRFSTicketBookingStatusAPIRes
frfsBookingStatus bookingId token = withFlowHandlerAPI $ verifyInternalToken token >> Domain.frfsBookingStatus bookingId
