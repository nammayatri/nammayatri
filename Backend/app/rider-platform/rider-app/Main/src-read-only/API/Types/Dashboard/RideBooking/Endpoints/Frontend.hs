{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.Dashboard.RideBooking.Endpoints.Frontend where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified "this" Domain.Types.Person
import qualified Kernel.Prelude
import qualified "this" Domain.Action.UI.Frontend
import qualified EulerHS.Types
import qualified Data.Singletons.TH



type API = ("frontend" :> (GetFrontendFlowStatus :<|> PostFrontendNotifyEvent))
type GetFrontendFlowStatus = ("flowStatus" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> QueryParam "isPolling" Kernel.Prelude.Bool :> QueryParam "checkForActiveBooking"
                                                                                                                                                                               Kernel.Prelude.Bool :> Get ('[JSON]) Domain.Action.UI.Frontend.GetPersonFlowStatusRes)
type PostFrontendNotifyEvent = ("notifyEvent" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> ReqBody ('[JSON]) Domain.Action.UI.Frontend.NotifyEventReq :> Post ('[JSON])
                                                                                                                                                                                            Domain.Action.UI.Frontend.NotifyEventResp)
data FrontendAPIs
    = FrontendAPIs {getFrontendFlowStatus :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> EulerHS.Types.EulerClient Domain.Action.UI.Frontend.GetPersonFlowStatusRes),
                    postFrontendNotifyEvent :: (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Frontend.NotifyEventReq -> EulerHS.Types.EulerClient Domain.Action.UI.Frontend.NotifyEventResp)}
mkFrontendAPIs :: (Client EulerHS.Types.EulerClient API -> FrontendAPIs)
mkFrontendAPIs frontendClient = (FrontendAPIs {..})
                   where getFrontendFlowStatus :<|> postFrontendNotifyEvent = frontendClient
data FrontendUserActionType
    = GET_FRONTEND_FLOW_STATUS | POST_FRONTEND_NOTIFY_EVENT
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''FrontendUserActionType)])

