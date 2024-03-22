{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FRFSTicketService where

import qualified API.Types.UI.FRFSTicketService
import qualified Control.Lens
import qualified Domain.Action.UI.FRFSTicketService as Domain.Action.UI.FRFSTicketService
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Station
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "frfs" :> "stations" :> QueryParam "city" Kernel.Types.Beckn.Context.City :> MandatoryQueryParam "vehicleType" Domain.Types.Station.FRFSVehicleType
      :> Get
           ('[JSON])
           [API.Types.UI.FRFSTicketService.FRFSStationAPI]
      :<|> TokenAuth
      :> "frfs"
      :> "search"
      :> MandatoryQueryParam
           "vehicleType"
           Domain.Types.Station.FRFSVehicleType
      :> ReqBody
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSSearchAPIReq
      :> Post
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
      :<|> TokenAuth
      :> "frfs"
      :> "search"
      :> Capture
           "searchId"
           (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch)
      :> "quote"
      :> Get
           ('[JSON])
           [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes]
      :<|> TokenAuth
      :> "frfs"
      :> "quote"
      :> Capture
           "quoteId"
           (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote)
      :> "confirm"
      :> Post
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
      :<|> TokenAuth
      :> "frfs"
      :> "quote"
      :> Capture
           "quoteId"
           (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote)
      :> "payment"
      :> "retry"
      :> Post
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
      :<|> TokenAuth
      :> "frfs"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "status"
      :> Get
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
      :<|> TokenAuth
      :> "frfs"
      :> "booking"
      :> "list"
      :> Get
           ('[JSON])
           [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
      :<|> TokenAuth
      :> "frfs"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "canCancel"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "frfs"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "canCancel"
      :> "status"
      :> Get
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSCanCancelStatus
      :<|> TokenAuth
      :> "frfs"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "cancel"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "frfs"
      :> "booking"
      :> "cancel"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "status"
      :> Get
           ('[JSON])
           API.Types.UI.FRFSTicketService.FRFSCancelStatus
  )

handler :: Environment.FlowServer API
handler = getFrfsStations :<|> postFrfsSearch :<|> getFrfsSearchQuote :<|> postFrfsQuoteConfirm :<|> postFrfsQuotePaymentRetry :<|> getFrfsBookingStatus :<|> getFrfsBookingList :<|> postFrfsBookingCanCancel :<|> getFrfsBookingCanCancelStatus :<|> postFrfsBookingCancel :<|> getFrfsBookingCancelStatus

getFrfsStations ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Types.Beckn.Context.City) ->
    Domain.Types.Station.FRFSVehicleType ->
    Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSStationAPI]
  )
getFrfsStations a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsStations (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postFrfsSearch ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Domain.Types.Station.FRFSVehicleType ->
    API.Types.UI.FRFSTicketService.FRFSSearchAPIReq ->
    Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
  )
postFrfsSearch a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsSearch (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getFrfsSearchQuote ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch ->
    Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes]
  )
getFrfsSearchQuote a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsSearchQuote (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postFrfsQuoteConfirm ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote ->
    Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
  )
postFrfsQuoteConfirm a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsQuoteConfirm (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postFrfsQuotePaymentRetry ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote ->
    Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
  )
postFrfsQuotePaymentRetry a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsQuotePaymentRetry (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getFrfsBookingStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking ->
    Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
  )
getFrfsBookingStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsBookingStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getFrfsBookingList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
  )
getFrfsBookingList a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsBookingList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postFrfsBookingCanCancel ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postFrfsBookingCanCancel a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsBookingCanCancel (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getFrfsBookingCanCancelStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking ->
    Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSCanCancelStatus
  )
getFrfsBookingCanCancelStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsBookingCanCancelStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postFrfsBookingCancel ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postFrfsBookingCancel a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsBookingCancel (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getFrfsBookingCancelStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking ->
    Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSCancelStatus
  )
getFrfsBookingCancelStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsBookingCancelStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
