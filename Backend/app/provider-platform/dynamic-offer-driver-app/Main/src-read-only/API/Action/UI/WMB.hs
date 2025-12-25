{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.WMB
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Driver
import qualified API.Types.UI.WMB
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.WMB
import qualified Domain.Types.AlertRequest
import qualified Domain.Types.FleetBadgeType
import qualified Domain.Types.FleetConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TripTransaction
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "wmb" :> "fleetBadges" :> QueryParam "mbSearchString" Data.Text.Text :> QueryParam "mbBadgeType" Domain.Types.FleetBadgeType.FleetBadgeType
      :> MandatoryQueryParam
           "limit"
           Kernel.Prelude.Int
      :> MandatoryQueryParam "offset" Kernel.Prelude.Int
      :> Get
           '[JSON]
           [API.Types.UI.WMB.AvailableBadge]
      :<|> TokenAuth
      :> "wmb"
      :> "availableRoutes"
      :> ReqBody
           '[JSON]
           API.Types.UI.WMB.AvailableRouteReq
      :> Post
           '[JSON]
           [API.Types.UI.WMB.AvailableRoute]
      :<|> TokenAuth
      :> "wmb"
      :> "qr"
      :> "start"
      :> ReqBody
           '[JSON]
           API.Types.UI.WMB.TripQrStartReq
      :> Post
           '[JSON]
           API.Types.UI.WMB.TripTransactionDetails
      :<|> TokenAuth
      :> "wmb"
      :> "trip"
      :> "active"
      :> Get
           '[JSON]
           API.Types.UI.WMB.ActiveTripTransaction
      :<|> TokenAuth
      :> "wmb"
      :> "route"
      :> Capture
           "routeCode"
           Data.Text.Text
      :> "details"
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Fleet.Endpoints.Driver.RouteDetails
      :<|> TokenAuth
      :> "wmb"
      :> "trip"
      :> "list"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "status"
           Domain.Types.TripTransaction.TripStatus
      :> Get
           '[JSON]
           [API.Types.UI.WMB.TripTransactionDetails]
      :<|> TokenAuth
      :> "wmb"
      :> "trip"
      :> Capture
           "tripTransactionId"
           (Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction)
      :> "start"
      :> ReqBody
           '[JSON]
           API.Types.UI.WMB.TripStartReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "wmb"
      :> "trip"
      :> Capture
           "tripTransactionId"
           (Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction)
      :> "end"
      :> ReqBody
           '[JSON]
           API.Types.UI.WMB.TripEndReq
      :> Post
           '[JSON]
           API.Types.UI.WMB.TripEndResp
      :<|> TokenAuth
      :> "wmb"
      :> "trip"
      :> Capture
           "tripTransactionId"
           (Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction)
      :> "request"
      :> ReqBody
           '[JSON]
           API.Types.UI.WMB.RequestDetails
      :> Post
           '[JSON]
           API.Types.UI.WMB.AlertReqResp
      :<|> TokenAuth
      :> "wmb"
      :> "requests"
      :> Capture
           "approvalRequestId"
           (Kernel.Types.Id.Id Domain.Types.AlertRequest.AlertRequest)
      :> "status"
      :> Get
           '[JSON]
           API.Types.UI.WMB.AlertRequestResp
      :<|> TokenAuth
      :> "wmb"
      :> "requests"
      :> Capture
           "approvalRequestId"
           (Kernel.Types.Id.Id Domain.Types.AlertRequest.AlertRequest)
      :> "cancel"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "fleet"
      :> "consent"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "fleet"
      :> "config"
      :> Get
           '[JSON]
           Domain.Types.FleetConfig.FleetConfig
  )

handler :: Environment.FlowServer API
handler = getWmbFleetBadges :<|> postWmbAvailableRoutes :<|> postWmbQrStart :<|> getWmbTripActive :<|> getWmbRouteDetails :<|> getWmbTripList :<|> postWmbTripStart :<|> postWmbTripEnd :<|> postWmbTripRequest :<|> getWmbRequestsStatus :<|> postWmbRequestsCancel :<|> postFleetConsent :<|> getFleetConfig

getWmbFleetBadges ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Kernel.Prelude.Maybe Domain.Types.FleetBadgeType.FleetBadgeType ->
    Kernel.Prelude.Int ->
    Kernel.Prelude.Int ->
    Environment.FlowHandler [API.Types.UI.WMB.AvailableBadge]
  )
getWmbFleetBadges a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.getWmbFleetBadges (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1

postWmbAvailableRoutes ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.WMB.AvailableRouteReq ->
    Environment.FlowHandler [API.Types.UI.WMB.AvailableRoute]
  )
postWmbAvailableRoutes a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.postWmbAvailableRoutes (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postWmbQrStart ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.WMB.TripQrStartReq ->
    Environment.FlowHandler API.Types.UI.WMB.TripTransactionDetails
  )
postWmbQrStart a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.postWmbQrStart (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getWmbTripActive ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.WMB.ActiveTripTransaction
  )
getWmbTripActive a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.getWmbTripActive (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getWmbRouteDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Endpoints.Driver.RouteDetails
  )
getWmbRouteDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.getWmbRouteDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getWmbTripList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Domain.Types.TripTransaction.TripStatus ->
    Environment.FlowHandler [API.Types.UI.WMB.TripTransactionDetails]
  )
getWmbTripList a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.getWmbTripList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

postWmbTripStart ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction ->
    API.Types.UI.WMB.TripStartReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postWmbTripStart a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.postWmbTripStart (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postWmbTripEnd ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction ->
    API.Types.UI.WMB.TripEndReq ->
    Environment.FlowHandler API.Types.UI.WMB.TripEndResp
  )
postWmbTripEnd a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.postWmbTripEnd (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postWmbTripRequest ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction ->
    API.Types.UI.WMB.RequestDetails ->
    Environment.FlowHandler API.Types.UI.WMB.AlertReqResp
  )
postWmbTripRequest a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.postWmbTripRequest (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getWmbRequestsStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.AlertRequest.AlertRequest ->
    Environment.FlowHandler API.Types.UI.WMB.AlertRequestResp
  )
getWmbRequestsStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.getWmbRequestsStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postWmbRequestsCancel ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.AlertRequest.AlertRequest ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postWmbRequestsCancel a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.postWmbRequestsCancel (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postFleetConsent ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postFleetConsent a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.postFleetConsent (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getFleetConfig ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler Domain.Types.FleetConfig.FleetConfig
  )
getFleetConfig a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.getFleetConfig (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
