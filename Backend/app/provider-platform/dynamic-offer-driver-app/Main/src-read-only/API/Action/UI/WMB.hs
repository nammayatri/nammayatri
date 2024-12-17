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
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
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
  ( TokenAuth :> "ui" :> "wmb" :> "availableRoutes" :> Capture "vehicleNumber" Data.Text.Text
      :> Get
           ('[JSON])
           [API.Types.UI.WMB.AvailableRoutesList]
      :<|> TokenAuth
      :> "ui"
      :> "wmb"
      :> "trip"
      :> "link"
      :> ReqBody ('[JSON]) API.Types.UI.WMB.TripLinkReq
      :> Post
           ('[JSON])
           API.Types.UI.WMB.TripLinkResp
      :<|> TokenAuth
      :> "ui"
      :> "wmb"
      :> "trip"
      :> "active"
      :> Get
           ('[JSON])
           API.Types.UI.WMB.TripLinkResp
      :<|> TokenAuth
      :> "ui"
      :> "wmb"
      :> "trip"
      :> Capture
           "tripTransactionId"
           Data.Text.Text
      :> "start"
      :> ReqBody
           ('[JSON])
           API.Types.UI.WMB.TripStartReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "ui"
      :> "wmb"
      :> "trip"
      :> Capture
           "tripTransactionId"
           Data.Text.Text
      :> "end"
      :> ReqBody
           ('[JSON])
           API.Types.UI.WMB.TripEndReq
      :> Post
           ('[JSON])
           API.Types.UI.WMB.TripEndResp
  )

handler :: Environment.FlowServer API
handler = getUiWmbAvailableRoutes :<|> postUiWmbTripLink :<|> getUiWmbTripActive :<|> postUiWmbTripStart :<|> postUiWmbTripEnd

getUiWmbAvailableRoutes ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler [API.Types.UI.WMB.AvailableRoutesList]
  )
getUiWmbAvailableRoutes a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.getUiWmbAvailableRoutes (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postUiWmbTripLink ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.WMB.TripLinkReq ->
    Environment.FlowHandler API.Types.UI.WMB.TripLinkResp
  )
postUiWmbTripLink a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.postUiWmbTripLink (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getUiWmbTripActive ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.WMB.TripLinkResp
  )
getUiWmbTripActive a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.getUiWmbTripActive (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postUiWmbTripStart ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    API.Types.UI.WMB.TripStartReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postUiWmbTripStart a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.postUiWmbTripStart (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postUiWmbTripEnd ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    API.Types.UI.WMB.TripEndReq ->
    Environment.FlowHandler API.Types.UI.WMB.TripEndResp
  )
postUiWmbTripEnd a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.WMB.postUiWmbTripEnd (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
