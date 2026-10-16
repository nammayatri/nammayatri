{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Dispatcher
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Dispatcher
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.Dispatcher
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
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth :> "dispatcher" :> Capture "fleetId" Data.Text.Text :> "getFleetInfo"
      :> Get
           '[JSON]
           API.Types.UI.Dispatcher.DispatcherRes
      :<|> TokenAuth
      :> "dispatcher"
      :> "updateFleetSchedule"
      :> ReqBody '[JSON] API.Types.UI.Dispatcher.DispatcherReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "dispatcher"
      :> "depots"
      :> Get
           '[JSON]
           [API.Types.UI.Dispatcher.DepotOption]
      :<|> TokenAuth
      :> "dispatcher"
      :> "getVehiclesByDepotId"
      :> Capture
           "depotId"
           Data.Text.Text
      :> Get
           '[JSON]
           [API.Types.UI.Dispatcher.DepotVehicle]
      :<|> TokenAuth
      :> "dispatcher"
      :> "getUserDepot"
      :> Get
           '[JSON]
           API.Types.UI.Dispatcher.UserDepotRes
      :<|> TokenAuth
      :> "dispatcher"
      :> "history"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           '[JSON]
           [API.Types.UI.Dispatcher.DispatcherHistoryRes]
  )

handler :: Environment.FlowServer API
handler = getDispatcherGetFleetInfo :<|> postDispatcherUpdateFleetSchedule :<|> getDispatcherDepots :<|> getDispatcherGetVehiclesByDepotId :<|> getDispatcherGetUserDepot :<|> getDispatcherHistory

getDispatcherGetFleetInfo ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler API.Types.UI.Dispatcher.DispatcherRes
  )
getDispatcherGetFleetInfo a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.Dispatcher.getDispatcherGetFleetInfo (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDispatcherUpdateFleetSchedule ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.Dispatcher.DispatcherReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDispatcherUpdateFleetSchedule a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.Dispatcher.postDispatcherUpdateFleetSchedule (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getDispatcherDepots ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler [API.Types.UI.Dispatcher.DepotOption]
  )
getDispatcherDepots a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.Dispatcher.getDispatcherDepots (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getDispatcherGetVehiclesByDepotId ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler [API.Types.UI.Dispatcher.DepotVehicle]
  )
getDispatcherGetVehiclesByDepotId a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.Dispatcher.getDispatcherGetVehiclesByDepotId (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getDispatcherGetUserDepot ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.Dispatcher.UserDepotRes
  )
getDispatcherGetUserDepot a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.Dispatcher.getDispatcherGetUserDepot (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getDispatcherHistory ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.FlowHandler [API.Types.UI.Dispatcher.DispatcherHistoryRes]
  )
getDispatcherHistory a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.Dispatcher.getDispatcherHistory (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
