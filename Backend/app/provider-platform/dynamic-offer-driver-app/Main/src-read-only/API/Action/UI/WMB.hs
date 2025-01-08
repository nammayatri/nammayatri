{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.WMB
  ( API,
    handler,
  )
where

import qualified API.Types.UI.WMB
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.WMB as Domain.Action.UI.WMB
import qualified Domain.Types.ApprovalRequest
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
  ( TokenAuth :> "wmb" :> "availableRoutes" :> ReqBody '[JSON] API.Types.UI.WMB.AvailableRouteReq
      :> Post
           '[JSON]
           [API.Types.UI.WMB.AvailableRoute]
      :<|> TokenAuth
      :> "wmb"
      :> "qr"
      :> "start"
      :> ReqBody '[JSON] API.Types.UI.WMB.TripQrStartReq
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
      :> "fleetconfig"
      :> Capture
           "fleetOwnerId"
           Data.Text.Text
      :> Get
           '[JSON]
           API.Types.UI.WMB.FleetConfigData
      :<|> TokenAuth
      :> "wmb"
      :> "route"
      :> Capture
           "routeCode"
           Data.Text.Text
      :> "details"
      :> Get
           '[JSON]
           API.Types.UI.WMB.RouteDetails
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
           API.Types.UI.WMB.DriverReqResp
      :<|> TokenAuth
      :> "wmb"
      :> "requests"
      :> Capture
           "approvalRequestId"
           (Kernel.Types.Id.Id Domain.Types.ApprovalRequest.ApprovalRequest)
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
  )

handler :: Environment.FlowServer API
handler = postWmbAvailableRoutes :<|> postWmbQrStart :<|> getWmbTripActive :<|> getWmbFleetconfig :<|> getWmbRouteDetails :<|> getWmbTripList :<|> postWmbTripStart :<|> postWmbTripEnd :<|> postWmbTripRequest :<|> postWmbRequestsCancel :<|> postFleetConsent

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

getWmbFleetconfig ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler API.Types.UI.WMB.FleetConfigData
  )
getWmbFleetconfig a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.getWmbFleetconfig (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getWmbRouteDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler API.Types.UI.WMB.RouteDetails
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
    Environment.FlowHandler API.Types.UI.WMB.DriverReqResp
  )
postWmbTripRequest a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.postWmbTripRequest (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postWmbRequestsCancel ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.ApprovalRequest.ApprovalRequest ->
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
