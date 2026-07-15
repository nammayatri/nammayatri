{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FRFSFleetOperator
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FRFSFleetOperator
import qualified BecknV2.FRFS.Enums
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.FRFSFleetOperator
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "v2" :> "frfs" :> "route" :> Capture "routeCode" Data.Text.Text :> QueryParam "integratedBppConfigId" Data.Text.Text
      :> QueryParam
           "platformType"
           Data.Text.Text
      :> MandatoryQueryParam "city" Kernel.Types.Beckn.Context.City
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> Get
           '[JSON]
           API.Types.UI.FRFSFleetOperator.FRFSRouteAPI
      :<|> TokenAuth
      :> "v2"
      :> "frfs"
      :> "trip"
      :> Capture
           "tripId"
           Data.Text.Text
      :> "route"
      :> Capture
           "routeId"
           Data.Text.Text
      :> "manifest"
      :> Get
           '[JSON]
           API.Types.UI.FRFSFleetOperator.FRFSTripPassengerManifestResp
      :<|> TokenAuth
      :> "frfs"
      :> "fleetOperator"
      :> "tripAction"
      :> ReqBody
           '[JSON]
           API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq
      :> Post
           '[JSON]
           API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp
      :<|> TokenAuth
      :> "frfs"
      :> "fleetOperator"
      :> "currentOperation"
      :> ReqBody
           '[JSON]
           API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq
      :> Post
           '[JSON]
           API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp
      :<|> TokenAuth
      :> "frfs"
      :> "fleetOperator"
      :> "search"
      :> MandatoryQueryParam
           "city"
           Kernel.Types.Beckn.Context.City
      :> MandatoryQueryParam
           "vehicleType"
           BecknV2.FRFS.Enums.VehicleCategory
      :> ReqBody
           '[JSON]
           API.Types.UI.FRFSFleetOperator.FRFSSearchAPIReq
      :> Post
           '[JSON]
           API.Types.UI.FRFSFleetOperator.FRFSSearchAPIRes
      :<|> TokenAuth
      :> "frfs"
      :> "fleetOperator"
      :> "search"
      :> Capture
           "searchId"
           Data.Text.Text
      :> "quote"
      :> Get
           '[JSON]
           [API.Types.UI.FRFSFleetOperator.FRFSQuoteAPIRes]
      :<|> TokenAuth
      :> "frfs"
      :> "fleetOperator"
      :> "quote"
      :> Capture
           "quoteId"
           Data.Text.Text
      :> "confirm"
      :> ReqBody
           '[JSON]
           API.Types.UI.FRFSFleetOperator.FRFSQuoteConfirmReq
      :> Post
           '[JSON]
           API.Types.UI.FRFSFleetOperator.FRFSTicketBookingStatusAPIRes
      :<|> TokenAuth
      :> "frfs"
      :> "fleetOperator"
      :> "booking"
      :> Capture
           "bookingId"
           Data.Text.Text
      :> "status"
      :> Get
           '[JSON]
           API.Types.UI.FRFSFleetOperator.FRFSTicketBookingStatusAPIRes
  )

handler :: Environment.FlowServer API
handler = getV2FrfsRoute :<|> getV2FrfsTripRouteManifest :<|> postFrfsFleetOperatorTripAction :<|> postFrfsFleetOperatorCurrentOperation :<|> postFrfsFleetOperatorSearch :<|> getFrfsFleetOperatorSearchQuote :<|> postFrfsFleetOperatorQuoteConfirm :<|> getFrfsFleetOperatorBookingStatus

getV2FrfsRoute ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Kernel.Types.Beckn.Context.City ->
    BecknV2.FRFS.Enums.VehicleCategory ->
    Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FRFSRouteAPI
  )
getV2FrfsRoute a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSFleetOperator.getV2FrfsRoute (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a6) a5 a4 a3 a2 a1

getV2FrfsTripRouteManifest ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Data.Text.Text ->
    Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FRFSTripPassengerManifestResp
  )
getV2FrfsTripRouteManifest a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSFleetOperator.getV2FrfsTripRouteManifest (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postFrfsFleetOperatorTripAction ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq ->
    Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp
  )
postFrfsFleetOperatorTripAction a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSFleetOperator.postFrfsFleetOperatorTripAction (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postFrfsFleetOperatorCurrentOperation ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq ->
    Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp
  )
postFrfsFleetOperatorCurrentOperation a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSFleetOperator.postFrfsFleetOperatorCurrentOperation (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postFrfsFleetOperatorSearch ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Beckn.Context.City ->
    BecknV2.FRFS.Enums.VehicleCategory ->
    API.Types.UI.FRFSFleetOperator.FRFSSearchAPIReq ->
    Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FRFSSearchAPIRes
  )
postFrfsFleetOperatorSearch a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSFleetOperator.postFrfsFleetOperatorSearch (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

getFrfsFleetOperatorSearchQuote ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler [API.Types.UI.FRFSFleetOperator.FRFSQuoteAPIRes]
  )
getFrfsFleetOperatorSearchQuote a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSFleetOperator.getFrfsFleetOperatorSearchQuote (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postFrfsFleetOperatorQuoteConfirm ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    API.Types.UI.FRFSFleetOperator.FRFSQuoteConfirmReq ->
    Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FRFSTicketBookingStatusAPIRes
  )
postFrfsFleetOperatorQuoteConfirm a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSFleetOperator.postFrfsFleetOperatorQuoteConfirm (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getFrfsFleetOperatorBookingStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FRFSTicketBookingStatusAPIRes
  )
getFrfsFleetOperatorBookingStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSFleetOperator.getFrfsFleetOperatorBookingStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
