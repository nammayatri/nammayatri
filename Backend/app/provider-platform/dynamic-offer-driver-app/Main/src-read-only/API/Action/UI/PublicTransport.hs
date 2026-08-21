{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PublicTransport
  ( API,
    handler,
  )
where

import qualified API.Types.UI.PublicTransport
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.PublicTransport
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

type API =
  ( TokenAuth :> "publicTransport" :> "vehicleData" :> Capture "vehicleNumber" Data.Text.Text :> "block" :> MandatoryQueryParam "isBlock" Kernel.Prelude.Bool
      :> Post
           ('[JSON])
           API.Types.UI.PublicTransport.BlockedVehiclesResp
      :<|> TokenAuth
      :> "publicTransport"
      :> "blockedVehicles"
      :> Get
           ('[JSON])
           API.Types.UI.PublicTransport.BlockedVehiclesResp
  )

handler :: Environment.FlowServer API
handler = postPublicTransportVehicleDataBlock :<|> getPublicTransportBlockedVehicles

postPublicTransportVehicleDataBlock ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Kernel.Prelude.Bool ->
    Environment.FlowHandler API.Types.UI.PublicTransport.BlockedVehiclesResp
  )
postPublicTransportVehicleDataBlock a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.PublicTransport.postPublicTransportVehicleDataBlock (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getPublicTransportBlockedVehicles ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler API.Types.UI.PublicTransport.BlockedVehiclesResp
  )
getPublicTransportBlockedVehicles a1 = withFlowHandlerAPI $ Domain.Action.UI.PublicTransport.getPublicTransportBlockedVehicles (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
