module API.UI.VehicleServiceTier
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.VehicleServiceTier as Domain
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

type API =
  "vehicleServiceTiers"
    :> TokenAuth
    :> Get '[JSON] [Domain.VehicleServiceTierInfo]

handler :: FlowServer API
handler = getVehicleServiceTiers

getVehicleServiceTiers :: (Id Person.Person, Id Merchant.Merchant) -> FlowHandler [Domain.VehicleServiceTierInfo]
getVehicleServiceTiers (riderId, merchantId) =
  withFlowHandlerAPIPersonId riderId . withPersonIdLogTag riderId $
    Domain.getVehicleServiceTiers merchantId riderId
