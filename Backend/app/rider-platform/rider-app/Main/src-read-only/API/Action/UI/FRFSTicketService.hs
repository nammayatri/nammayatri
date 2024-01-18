{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FRFSTicketService where

import API.Types.UI.FRFSTicketService (FRFSBookingPaymentAPI, FRFSBookingPaymentStatusAPI, FRFSBookingStatusAPIRes, FRFSQuoteAPI, FRFSSearchAPIReq, FRFSSearchAPIRes, FRFSStationAPI, FRFSTicketAPI)
import qualified Domain.Action.UI.FRFSTicketService as Domain.Action.UI.FRFSTicketService
import qualified Domain.Types.FRFSBookingStatusAPIRes
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSQuoteAPI
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSSearchAPIReq
import qualified Domain.Types.FRFSSearchAPIRes
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatusAPIRes
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Station
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  TokenAuth :> "frfs" :> "stations" :> QueryParam "vehicleType" (Kernel.Prelude.Text) :> Get '[JSON] [Domain.Types.Station.Station]
    :<|> TokenAuth :> "frfs" :> "search" :> QueryParam "vehicleType" (Kernel.Prelude.Text) :> ReqBody '[JSON] Domain.Types.FRFSSearchAPIReq.FRFSSearchAPIReq :> Post '[JSON] Domain.Types.FRFSSearchAPIRes.FRFSSearchAPIRes
    :<|> TokenAuth :> "frfs" :> "search" :> Capture "searchId" (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch) :> "quote" :> Get '[JSON] [Domain.Types.FRFSQuoteAPI.FRFSQuoteAPI]
    :<|> TokenAuth :> "frfs" :> "quote" :> Capture "quoteId" (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote) :> "confirm" :> Post '[JSON] Domain.Types.FRFSTicketBookingStatusAPIRes.FRFSTicketBookingStatusAPIRes
    :<|> TokenAuth :> "frfs" :> "quote" :> Capture "quoteId" (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote) :> "payment" :> "retry" :> Post '[JSON] Domain.Types.FRFSTicketBookingStatusAPIRes.FRFSTicketBookingStatusAPIRes
    :<|> TokenAuth :> "frfs" :> "booking" :> Capture "bookingId" (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking) :> "status" :> Get '[JSON] Domain.Types.FRFSTicketBookingStatusAPIRes.FRFSTicketBookingStatusAPIRes
    :<|> TokenAuth :> "frfs" :> "booking" :> "list" :> Get '[JSON] [Domain.Types.FRFSBookingStatusAPIRes.FRFSBookingStatusAPIRes]

handler :: Environment.FlowServer API
handler =
  getFrfsStations
    :<|> postFrfsSearch
    :<|> getFrfsSearchQuote
    :<|> postFrfsQuoteConfirm
    :<|> postFrfsQuotePaymentRetry
    :<|> getFrfsBookingStatus
    :<|> getFrfsBookingList

getFrfsStations :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler [Domain.Types.Station.Station]
getFrfsStations a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsStations (Kernel.Prelude.first Kernel.Prelude.Just a2) a1

postFrfsSearch :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Domain.Types.FRFSSearchAPIReq.FRFSSearchAPIReq -> Environment.FlowHandler Domain.Types.FRFSSearchAPIRes.FRFSSearchAPIRes
postFrfsSearch a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsSearch (Kernel.Prelude.first Kernel.Prelude.Just a3) a2 a1

getFrfsSearchQuote :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> Environment.FlowHandler [Domain.Types.FRFSQuoteAPI.FRFSQuoteAPI]
getFrfsSearchQuote a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsSearchQuote (Kernel.Prelude.first Kernel.Prelude.Just a2) a1

postFrfsQuoteConfirm :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Environment.FlowHandler Domain.Types.FRFSTicketBookingStatusAPIRes.FRFSTicketBookingStatusAPIRes
postFrfsQuoteConfirm a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsQuoteConfirm (Kernel.Prelude.first Kernel.Prelude.Just a2) a1

postFrfsQuotePaymentRetry :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> Environment.FlowHandler Domain.Types.FRFSTicketBookingStatusAPIRes.FRFSTicketBookingStatusAPIRes
postFrfsQuotePaymentRetry a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsQuotePaymentRetry (Kernel.Prelude.first Kernel.Prelude.Just a2) a1

getFrfsBookingStatus :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.FlowHandler Domain.Types.FRFSTicketBookingStatusAPIRes.FRFSTicketBookingStatusAPIRes
getFrfsBookingStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsBookingStatus (Kernel.Prelude.first Kernel.Prelude.Just a2) a1

getFrfsBookingList :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [Domain.Types.FRFSBookingStatusAPIRes.FRFSBookingStatusAPIRes]
getFrfsBookingList a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsBookingList (Kernel.Prelude.first Kernel.Prelude.Just a1)
