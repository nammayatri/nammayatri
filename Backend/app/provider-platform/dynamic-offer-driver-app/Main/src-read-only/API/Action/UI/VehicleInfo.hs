{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.VehicleInfo
  ( API,
    handler,
  )
where

import qualified API.Types.UI.VehicleInfo
import qualified Control.Lens
import qualified Domain.Action.UI.VehicleInfo as Domain.Action.UI.VehicleInfo
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleInfo
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
  ( TokenAuth :> "vehicleInfo" :> "list" :> Get ('[JSON]) [Domain.Types.VehicleInfo.VehicleInfo] :<|> TokenAuth :> "vehicleInfo" :> "update"
      :> ReqBody
           ('[JSON])
           API.Types.UI.VehicleInfo.UpdateVehicleInfoReq
      :> Put ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getVehicleInfoList :<|> putVehicleInfoUpdate

getVehicleInfoList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler [Domain.Types.VehicleInfo.VehicleInfo]
  )
getVehicleInfoList a1 = withFlowHandlerAPI $ Domain.Action.UI.VehicleInfo.getVehicleInfoList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

putVehicleInfoUpdate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.VehicleInfo.UpdateVehicleInfoReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
putVehicleInfoUpdate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.VehicleInfo.putVehicleInfoUpdate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
