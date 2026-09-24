{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Frontend where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Frontend
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("frontend" :> (GetFrontendFlowStatus :<|> PostFrontendNotifyEvent))

type GetFrontendFlowStatus =
  ( "flowStatus" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> QueryParam "isPolling" Kernel.Prelude.Bool
      :> QueryParam
           "checkForActiveBooking"
           Kernel.Prelude.Bool
      :> Get '[JSON] Domain.Action.UI.Frontend.GetPersonFlowStatusRes
  )

type PostFrontendNotifyEvent =
  ( "notifyEvent" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> ReqBody '[JSON] Domain.Action.UI.Frontend.NotifyEventReq
      :> Post
           '[JSON]
           Domain.Action.UI.Frontend.NotifyEventResp
  )

data FrontendAPIs = FrontendAPIs
  { getFrontendFlowStatus :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> EulerHS.Types.EulerClient Domain.Action.UI.Frontend.GetPersonFlowStatusRes,
    postFrontendNotifyEvent :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Frontend.NotifyEventReq -> EulerHS.Types.EulerClient Domain.Action.UI.Frontend.NotifyEventResp
  }

mkFrontendAPIs :: (Client EulerHS.Types.EulerClient API -> FrontendAPIs)
mkFrontendAPIs frontendClient = (FrontendAPIs {..})
  where
    getFrontendFlowStatus :<|> postFrontendNotifyEvent = frontendClient

data FrontendUserActionType
  = GET_FRONTEND_FLOW_STATUS
  | POST_FRONTEND_NOTIFY_EVENT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''FrontendUserActionType])
