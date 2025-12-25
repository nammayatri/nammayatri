{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.VehicleDetails
  ( API,
    handler,
  )
where

import qualified API.Types.UI.VehicleDetails
import qualified Control.Lens
import qualified Domain.Action.UI.VehicleDetails
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleDetails
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "vehicleMakes" :> Get '[JSON] API.Types.UI.VehicleDetails.VehicleMakesResp :<|> TokenAuth :> "vehicleModels"
      :> ReqBody
           '[JSON]
           API.Types.UI.VehicleDetails.VehicleModelsReq
      :> Post
           '[JSON]
           API.Types.UI.VehicleDetails.VehicleModelsResp
      :<|> TokenAuth
      :> "vehicleDetails"
      :> ReqBody
           '[JSON]
           API.Types.UI.VehicleDetails.VehicleDetailsReq
      :> Post
           '[JSON]
           Domain.Types.VehicleDetails.VehicleDetails
  )

handler :: Environment.FlowServer API
handler = getVehicleMakes :<|> postVehicleModels :<|> postVehicleDetails

getVehicleMakes ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.VehicleDetails.VehicleMakesResp
  )
getVehicleMakes a1 = withFlowHandlerAPI $ Domain.Action.UI.VehicleDetails.getVehicleMakes (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postVehicleModels ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.VehicleDetails.VehicleModelsReq ->
    Environment.FlowHandler API.Types.UI.VehicleDetails.VehicleModelsResp
  )
postVehicleModels a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.VehicleDetails.postVehicleModels (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postVehicleDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.VehicleDetails.VehicleDetailsReq ->
    Environment.FlowHandler Domain.Types.VehicleDetails.VehicleDetails
  )
postVehicleDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.VehicleDetails.postVehicleDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
