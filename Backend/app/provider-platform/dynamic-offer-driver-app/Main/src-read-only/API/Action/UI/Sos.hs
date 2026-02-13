{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Sos
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Sos
import qualified Control.Lens
import qualified Domain.Action.UI.Sos
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Safety.Domain.Types.Sos
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "sos" :> "getDetails" :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
      :> Get
           '[JSON]
           API.Types.UI.Sos.SosDetailsRes
      :<|> TokenAuth
      :> "sos"
      :> "create"
      :> ReqBody '[JSON] API.Types.UI.Sos.SosReq
      :> Post
           '[JSON]
           API.Types.UI.Sos.SosRes
      :<|> TokenAuth
      :> "sos"
      :> "markRideAsSafe"
      :> Capture
           "sosId"
           (Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos)
      :> ReqBody
           '[JSON]
           API.Types.UI.Sos.MarkAsSafeReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getSosGetDetails :<|> postSosCreate :<|> postSosMarkRideAsSafe

getSosGetDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.FlowHandler API.Types.UI.Sos.SosDetailsRes
  )
getSosGetDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.getSosGetDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postSosCreate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.Sos.SosReq ->
    Environment.FlowHandler API.Types.UI.Sos.SosRes
  )
postSosCreate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.postSosCreate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postSosMarkRideAsSafe ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Safety.Domain.Types.Sos.Sos ->
    API.Types.UI.Sos.MarkAsSafeReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSosMarkRideAsSafe a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.postSosMarkRideAsSafe (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
