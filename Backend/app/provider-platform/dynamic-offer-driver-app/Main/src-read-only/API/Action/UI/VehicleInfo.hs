{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.VehicleInfo
  ( API,
    handler,
  )
where

import qualified API.Types.UI.VehicleInfo
import qualified Control.Lens
import qualified Domain.Action.UI.VehicleInfo
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "vehicleInfo" :> Capture "rcNo" Kernel.Prelude.Text :> "list" :> Get '[JSON] API.Types.UI.VehicleInfo.VehicleExtraInformation)

handler :: Environment.FlowServer API
handler = getVehicleInfoList

getVehicleInfoList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.VehicleInfo.VehicleExtraInformation
  )
getVehicleInfoList a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.VehicleInfo.getVehicleInfoList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
