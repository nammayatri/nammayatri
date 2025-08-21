{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Sos
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Sos
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.Sos
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
  ( TokenAuth :> "sos" :> "getDetails" :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
      :> Get
           '[JSON]
           API.Types.UI.Sos.SosDetailsRes
      :<|> "sos"
      :> "IvrOutcome"
      :> QueryParam "CallFrom" Data.Text.Text
      :> QueryParam "CallSid" Data.Text.Text
      :> QueryParam
           "CallStatus"
           Data.Text.Text
      :> QueryParam
           "digits"
           Data.Text.Text
      :> Get
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "sos"
      :> "create"
      :> ReqBody
           '[JSON]
           API.Types.UI.Sos.SosReq
      :> Post
           '[JSON]
           API.Types.UI.Sos.SosRes
      :<|> TokenAuth
      :> "sos"
      :> Capture
           "sosId"
           (Kernel.Types.Id.Id Domain.Types.Sos.Sos)
      :> "status"
      :> ReqBody
           '[JSON]
           API.Types.UI.Sos.SosUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "sos"
      :> "markRideAsSafe"
      :> Capture
           "sosId"
           (Kernel.Types.Id.Id Domain.Types.Sos.Sos)
      :> ReqBody
           '[JSON]
           API.Types.UI.Sos.MarkAsSafeReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "sos"
      :> "createMockSos"
      :> ReqBody
           '[JSON]
           API.Types.UI.Sos.MockSosReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "sos"
      :> "callPolice"
      :> ReqBody
           '[JSON]
           API.Types.UI.Sos.CallPoliceAPI
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getSosGetDetails :<|> getSosIvrOutcome :<|> postSosCreate :<|> postSosStatus :<|> postSosMarkRideAsSafe :<|> postSosCreateMockSos :<|> postSosCallPolice

getSosGetDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.FlowHandler API.Types.UI.Sos.SosDetailsRes
  )
getSosGetDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.getSosGetDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getSosIvrOutcome :: (Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Data.Text.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
getSosIvrOutcome a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.getSosIvrOutcome a4 a3 a2 a1

postSosCreate :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> API.Types.UI.Sos.SosReq -> Environment.FlowHandler API.Types.UI.Sos.SosRes)
postSosCreate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.postSosCreate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postSosStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Sos.Sos ->
    API.Types.UI.Sos.SosUpdateReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSosStatus a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.postSosStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postSosMarkRideAsSafe ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Sos.Sos ->
    API.Types.UI.Sos.MarkAsSafeReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSosMarkRideAsSafe a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.postSosMarkRideAsSafe (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postSosCreateMockSos ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.Sos.MockSosReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSosCreateMockSos a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.postSosCreateMockSos (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postSosCallPolice ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.Sos.CallPoliceAPI ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSosCallPolice a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Sos.postSosCallPolice (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
