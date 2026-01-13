{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FRFSTicketService
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FRFSTicketService
import qualified BecknV2.FRFS.Enums
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.FRFSTicketService
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "frfs" :> "config" :> MandatoryQueryParam "city" Kernel.Types.Beckn.Context.City
      :> Get
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSConfigAPIRes
      :<|> TokenAuth
      :> "frfs"
      :> "autocomplete"
      :> QueryParam "input" Data.Text.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "platformType"
           Domain.Types.IntegratedBPPConfig.PlatformType
      :> MandatoryQueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> MandatoryQueryParam
           "location"
           Kernel.External.Maps.Types.LatLong
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           '[JSON]
           API.Types.UI.FRFSTicketService.AutocompleteRes
      :<|> TokenAuth
      :> "frfs"
      :> "routes"
      :> QueryParam
           "endStationCode"
           Data.Text.Text
      :> QueryParam
           "startStationCode"
           Data.Text.Text
      :> MandatoryQueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           '[JSON]
           [API.Types.UI.FRFSTicketService.FRFSRouteAPI]
      :<|> TokenAuth
      :> "frfs"
      :> "stations"
      :> QueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> QueryParam
           "endStationCode"
           Data.Text.Text
      :> QueryParam
           "location"
           Kernel.External.Maps.Types.LatLong
      :> QueryParam
           "minimalData"
           Kernel.Prelude.Bool
      :> QueryParam
           "platformType"
           Domain.Types.IntegratedBPPConfig.PlatformType
      :> QueryParam
           "routeCode"
           Data.Text.Text
      :> QueryParam
           "startStationCode"
           Data.Text.Text
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           '[JSON]
           [API.Types.UI.FRFSTicketService.FRFSStationAPI]
      :<|> TokenAuth
      :> "frfs"
      :> "stations"
      :> "possibleStops"
      :> QueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> QueryParam
           "platformType"
           Domain.Types.IntegratedBPPConfig.PlatformType
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> ReqBody
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSPossibleStopsReq
      :> Post
           '[JSON]
           [API.Types.UI.FRFSTicketService.FRFSStationAPI]
      :<|> TokenAuth
      :> "frfs"
      :> "route"
      :> Capture
           "routeCode"
           Data.Text.Text
      :> QueryParam
           "integratedBppConfigId"
           (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig)
      :> QueryParam
           "platformType"
           Domain.Types.IntegratedBPPConfig.PlatformType
      :> MandatoryQueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSRouteAPI
      :<|> TokenAuth
      :> "frfs"
      :> "search"
      :> QueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> QueryParam
           "integratedBppConfigId"
           (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig)
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> ReqBody
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSSearchAPIReq
      :> Post
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
      :<|> TokenAuth
      :> "frfs"
      :> "discovery"
      :> "search"
      :> QueryParam
           "integratedBppConfigId"
           (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig)
      :> ReqBody
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSDiscoverySearchAPIReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "frfs"
      :> "search"
      :> Capture
           "searchId"
           (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch)
      :> "quote"
      :> Get
           '[JSON]
           [API.Types.UI.FRFSTicketService.FRFSQuoteAPIRes]
      :<|> TokenAuth
      :> "frfs"
      :> "quote"
      :> Capture
           "quoteId"
           (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote)
      :> "confirm"
      :> QueryParam
           "isMockPayment"
           Kernel.Prelude.Bool
      :> Post
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
      :<|> TokenAuth
      :> "frfs"
      :> "quote"
      :> "v2"
      :> Capture
           "quoteId"
           (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote)
      :> "confirm"
      :> QueryParam
           "isMockPayment"
           Kernel.Prelude.Bool
      :> ReqBody
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq
      :> Post
           '[JSON]
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
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
      :<|> TokenAuth
      :> "frfs"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "status"
      :> Get
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
      :<|> TokenAuth
      :> "frfs"
      :> "booking"
      :> "list"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           '[JSON]
           [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
      :<|> TokenAuth
      :> "frfs"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "canCancel"
      :> Post
           '[JSON]
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
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSCanCancelStatus
      :<|> TokenAuth
      :> "frfs"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "cancel"
      :> Post
           '[JSON]
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
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSCancelStatus
      :<|> TokenAuth
      :> "frfs"
      :> "ticket"
      :> "verify"
      :> QueryParam
           "platformType"
           Domain.Types.IntegratedBPPConfig.PlatformType
      :> MandatoryQueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> ReqBody
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSTicketVerifyReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "frfs"
      :> "booking"
      :> Capture
           "bookingId"
           (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking)
      :> "feedback"
      :> ReqBody
           '[JSON]
           API.Types.UI.FRFSTicketService.FRFSBookingFeedbackReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getFrfsConfig :<|> getFrfsAutocomplete :<|> getFrfsRoutes :<|> getFrfsStations :<|> postFrfsStationsPossibleStops :<|> getFrfsRoute :<|> postFrfsSearch :<|> postFrfsDiscoverySearch :<|> getFrfsSearchQuote :<|> postFrfsQuoteConfirm :<|> postFrfsQuoteV2Confirm :<|> postFrfsQuotePaymentRetry :<|> getFrfsBookingStatus :<|> getFrfsBookingList :<|> postFrfsBookingCanCancel :<|> getFrfsBookingCanCancelStatus :<|> postFrfsBookingCancel :<|> getFrfsBookingCancelStatus :<|> postFrfsTicketVerify :<|> postFrfsBookingFeedback

getFrfsConfig ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Beckn.Context.City ->
    Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSConfigAPIRes
  )
getFrfsConfig a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsConfig (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getFrfsAutocomplete ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType ->
    Kernel.Types.Beckn.Context.City ->
    Kernel.External.Maps.Types.LatLong ->
    BecknV2.FRFS.Enums.VehicleCategory ->
    Environment.FlowHandler API.Types.UI.FRFSTicketService.AutocompleteRes
  )
getFrfsAutocomplete a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsAutocomplete (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a8) a7 a6 a5 a4 a3 a2 a1

getFrfsRoutes ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Kernel.Types.Beckn.Context.City ->
    BecknV2.FRFS.Enums.VehicleCategory ->
    Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSRouteAPI]
  )
getFrfsRoutes a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsRoutes (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1

getFrfsStations ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    BecknV2.FRFS.Enums.VehicleCategory ->
    Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSStationAPI]
  )
getFrfsStations a9 a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsStations (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a9) a8 a7 a6 a5 a4 a3 a2 a1

postFrfsStationsPossibleStops ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City ->
    Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType ->
    BecknV2.FRFS.Enums.VehicleCategory ->
    API.Types.UI.FRFSTicketService.FRFSPossibleStopsReq ->
    Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSStationAPI]
  )
postFrfsStationsPossibleStops a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsStationsPossibleStops (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1

getFrfsRoute ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Data.Text.Text ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) ->
    Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType ->
    Kernel.Types.Beckn.Context.City ->
    BecknV2.FRFS.Enums.VehicleCategory ->
    Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSRouteAPI
  )
getFrfsRoute a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsRoute (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a6) a5 a4 a3 a2 a1

postFrfsSearch ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.City ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) ->
    BecknV2.FRFS.Enums.VehicleCategory ->
    API.Types.UI.FRFSTicketService.FRFSSearchAPIReq ->
    Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSSearchAPIRes
  )
postFrfsSearch a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsSearch (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1

postFrfsDiscoverySearch ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig) ->
    API.Types.UI.FRFSTicketService.FRFSDiscoverySearchAPIReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postFrfsDiscoverySearch a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsDiscoverySearch (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

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
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
  )
postFrfsQuoteConfirm a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsQuoteConfirm (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postFrfsQuoteV2Confirm ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    API.Types.UI.FRFSTicketService.FRFSQuoteConfirmReq ->
    Environment.FlowHandler API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes
  )
postFrfsQuoteV2Confirm a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsQuoteV2Confirm (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

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
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe BecknV2.FRFS.Enums.VehicleCategory ->
    Environment.FlowHandler [API.Types.UI.FRFSTicketService.FRFSTicketBookingStatusAPIRes]
  )
getFrfsBookingList a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.getFrfsBookingList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

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

postFrfsTicketVerify ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Domain.Types.IntegratedBPPConfig.PlatformType ->
    Kernel.Types.Beckn.Context.City ->
    BecknV2.FRFS.Enums.VehicleCategory ->
    API.Types.UI.FRFSTicketService.FRFSTicketVerifyReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postFrfsTicketVerify a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsTicketVerify (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1

postFrfsBookingFeedback ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking ->
    API.Types.UI.FRFSTicketService.FRFSBookingFeedbackReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postFrfsBookingFeedback a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSTicketService.postFrfsBookingFeedback (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
