{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.VehicleDetails where

import qualified API.Types.UI.VehicleDetails
import qualified Control.Lens
import qualified Domain.Action.UI.VehicleDetails as Domain.Action.UI.VehicleDetails
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
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
  ( TokenAuth :> "vehicleMakes" :> Get ('[JSON]) API.Types.UI.VehicleDetails.VehicleModelsResp :<|> TokenAuth :> "vehicleModels"
      :> ReqBody
           ('[JSON])
           API.Types.UI.VehicleDetails.VehicleVariantsReq
      :> Get
           ('[JSON])
           API.Types.UI.VehicleDetails.VehicleVariantsResp
      :<|> TokenAuth
      :> "vehicleDetails"
      :> ReqBody
           ('[JSON])
           API.Types.UI.VehicleDetails.VehicleDetailsReq
      :> Get
           ('[JSON])
           Domain.Types.VehicleDetails.VehicleDetails
  )

handler :: Environment.FlowServer API
handler = getVehicleMakes :<|> getVehicleModels :<|> getVehicleDetails

getVehicleMakes ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.VehicleDetails.VehicleModelsResp
  )
getVehicleMakes a1 = withFlowHandlerAPI $ Domain.Action.UI.VehicleDetails.getVehicleMakes (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getVehicleModels ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.VehicleDetails.VehicleVariantsReq ->
    Environment.FlowHandler API.Types.UI.VehicleDetails.VehicleVariantsResp
  )
getVehicleModels a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.VehicleDetails.getVehicleModels (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getVehicleDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.VehicleDetails.VehicleDetailsReq ->
    Environment.FlowHandler Domain.Types.VehicleDetails.VehicleDetails
  )
getVehicleDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.VehicleDetails.getVehicleDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
