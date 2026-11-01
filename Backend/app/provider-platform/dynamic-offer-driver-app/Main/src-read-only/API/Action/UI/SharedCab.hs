{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SharedCab
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SharedCab
import qualified Control.Lens
import qualified Domain.Action.UI.Call
import qualified Domain.Action.UI.SharedCab
import qualified Domain.Types.CallStatus
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.SharedCabSession
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
  ( TokenAuth :> "publicTransport" :> "data" :> Get ('[JSON]) Domain.Types.SharedCabSession.SharedCabRouteDataResp :<|> TokenAuth :> "sharedCab" :> "routes" :> "demand"
      :> ReqBody
           ('[JSON])
           API.Types.UI.SharedCab.SharedCabDemandReq
      :> Post
           ('[JSON])
           Domain.Types.SharedCabSession.SharedCabDemandResp
      :<|> TokenAuth
      :> "sharedCab"
      :> "route"
      :> "operations"
      :> ReqBody
           ('[JSON])
           API.Types.UI.SharedCab.SharedCabRouteOperationReq
      :> Post
           ('[JSON])
           API.Types.UI.SharedCab.SelectRouteResp
      :<|> TokenAuth
      :> "sharedCab"
      :> "session"
      :> Get
           ('[JSON])
           ((Kernel.Prelude.Maybe Domain.Types.SharedCabSession.SharedCabSession))
      :<|> TokenAuth
      :> "sharedCab"
      :> "session"
      :> "operations"
      :> ReqBody
           ('[JSON])
           API.Types.UI.SharedCab.SharedCabSessionOperationReq
      :> Post
           ('[JSON])
           Domain.Types.SharedCabSession.SharedCabSession
      :<|> TokenAuth
      :> "sharedCab"
      :> "booking"
      :> Capture
           "bookingId"
           Kernel.Prelude.Text
      :> "operations"
      :> ReqBody
           ('[JSON])
           API.Types.UI.SharedCab.SharedCabBookingOperationReq
      :> Post
           ('[JSON])
           Domain.Types.SharedCabSession.SharedCabSession
      :<|> TokenAuth
      :> "sharedCab"
      :> "booking"
      :> Capture
           "bookingId"
           Kernel.Prelude.Text
      :> "call"
      :> Post
           ('[JSON])
           Domain.Action.UI.Call.CallRes
      :<|> TokenAuth
      :> "sharedCab"
      :> "booking"
      :> Capture
           "bookingId"
           Kernel.Prelude.Text
      :> "message"
      :> ReqBody
           ('[JSON])
           API.Types.UI.SharedCab.SharedCabMessageReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "sharedCab"
      :> "call"
      :> Capture
           "callId"
           (Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus)
      :> "status"
      :> Get
           ('[JSON])
           Domain.Action.UI.Call.GetCallStatusRes
  )

handler :: Environment.FlowServer API
handler = getPublicTransportData :<|> postSharedCabRoutesDemand :<|> postSharedCabRouteOperations :<|> getSharedCabSession :<|> postSharedCabSessionOperations :<|> postSharedCabBookingOperations :<|> postSharedCabBookingCall :<|> postSharedCabBookingMessage :<|> getSharedCabCallStatus

getPublicTransportData ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler Domain.Types.SharedCabSession.SharedCabRouteDataResp
  )
getPublicTransportData a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.SharedCab.getPublicTransportData (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postSharedCabRoutesDemand ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.SharedCab.SharedCabDemandReq ->
    Environment.FlowHandler Domain.Types.SharedCabSession.SharedCabDemandResp
  )
postSharedCabRoutesDemand a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.SharedCab.postSharedCabRoutesDemand (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postSharedCabRouteOperations ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.SharedCab.SharedCabRouteOperationReq ->
    Environment.FlowHandler API.Types.UI.SharedCab.SelectRouteResp
  )
postSharedCabRouteOperations a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.SharedCab.postSharedCabRouteOperations (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getSharedCabSession ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler (Kernel.Prelude.Maybe Domain.Types.SharedCabSession.SharedCabSession)
  )
getSharedCabSession a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a1) $ Domain.Action.UI.SharedCab.getSharedCabSession (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postSharedCabSessionOperations ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.SharedCab.SharedCabSessionOperationReq ->
    Environment.FlowHandler Domain.Types.SharedCabSession.SharedCabSession
  )
postSharedCabSessionOperations a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.SharedCab.postSharedCabSessionOperations (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postSharedCabBookingOperations ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    API.Types.UI.SharedCab.SharedCabBookingOperationReq ->
    Environment.FlowHandler Domain.Types.SharedCabSession.SharedCabSession
  )
postSharedCabBookingOperations a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.SharedCab.postSharedCabBookingOperations (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postSharedCabBookingCall ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler Domain.Action.UI.Call.CallRes
  )
postSharedCabBookingCall a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.SharedCab.postSharedCabBookingCall (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postSharedCabBookingMessage ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    API.Types.UI.SharedCab.SharedCabMessageReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSharedCabBookingMessage a3 a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a3) $ Domain.Action.UI.SharedCab.postSharedCabBookingMessage (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getSharedCabCallStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.CallStatus.CallStatus ->
    Environment.FlowHandler Domain.Action.UI.Call.GetCallStatusRes
  )
getSharedCabCallStatus a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.SharedCab.getSharedCabCallStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
