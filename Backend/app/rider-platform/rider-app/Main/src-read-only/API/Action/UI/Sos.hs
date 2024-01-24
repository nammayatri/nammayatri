{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Sos where

import API.Types.UI.Sos (SosDetailsRes, SosReq, SosRes, SosUpdateReq)
import qualified API.Types.UI.Sos
import qualified Domain.Action.UI.Sos as Domain.Action.UI.Sos
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Domain.Types.Sos
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
  TokenAuth :> "sos" :> "getDetails" :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride) :> Get '[JSON] API.Types.UI.Sos.SosDetailsRes
    :<|> TokenAuth :> "sos" :> "create" :> ReqBody '[JSON] API.Types.UI.Sos.SosReq :> Post '[JSON] API.Types.UI.Sos.SosRes
    :<|> TokenAuth :> "sos" :> Capture "sosId" (Kernel.Types.Id.Id Domain.Types.Sos.Sos) :> "status" :> ReqBody '[JSON] API.Types.UI.Sos.SosUpdateReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
    :<|> TokenAuth :> "sos" :> "markRideAsSafe" :> Capture "sosId" (Kernel.Types.Id.Id Domain.Types.Sos.Sos) :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
    :<|> TokenAuth :> "sos" :> "createMockSos" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess

handler :: Environment.FlowServer API
handler =
  getSosGetDetails
    :<|> postSosCreate
    :<|> postSosStatus
    :<|> postSosMarkRideAsSafe
    :<|> postSosCreateMockSos

getSosGetDetails :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Environment.FlowHandler API.Types.UI.Sos.SosDetailsRes
getSosGetDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.getSosGetDetails (Kernel.Prelude.first Kernel.Prelude.Just a2) a1

postSosCreate :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.Sos.SosReq -> Environment.FlowHandler API.Types.UI.Sos.SosRes
postSosCreate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.postSosCreate (Kernel.Prelude.first Kernel.Prelude.Just a2) a1

postSosStatus :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.Sos.Sos -> API.Types.UI.Sos.SosUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postSosStatus a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.postSosStatus (Kernel.Prelude.first Kernel.Prelude.Just a3) a2 a1

postSosMarkRideAsSafe :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.Sos.Sos -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postSosMarkRideAsSafe a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.postSosMarkRideAsSafe (Kernel.Prelude.first Kernel.Prelude.Just a2) a1

postSosCreateMockSos :: (Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postSosCreateMockSos a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.postSosCreateMockSos (Kernel.Prelude.first Kernel.Prelude.Just a1)
