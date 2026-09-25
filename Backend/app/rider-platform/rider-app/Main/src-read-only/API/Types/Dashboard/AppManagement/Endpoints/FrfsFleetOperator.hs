{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.FrfsFleetOperator where

import qualified "this" API.Types.UI.FRFSTicketService
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import Kernel.Types.Common
import Servant
import Servant.Client

type API = ("FrfsFleetOperator" :> (PostFrfsFleetOperatorCurrentOperation :<|> PostFrfsFleetOperatorTripAction))

type PostFrfsFleetOperatorCurrentOperation =
  ( "currentOperation" :> ReqBody ('[JSON]) API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationReq
      :> Post
           ('[JSON])
           API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationResp
  )

type PostFrfsFleetOperatorTripAction =
  ( "tripAction" :> ReqBody ('[JSON]) API.Types.UI.FRFSTicketService.FleetOperatorTripActionReq
      :> Post
           ('[JSON])
           API.Types.UI.FRFSTicketService.FleetOperatorTripActionResp
  )

data FrfsFleetOperatorAPIs = FrfsFleetOperatorAPIs
  { postFrfsFleetOperatorCurrentOperation :: (API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationReq -> EulerHS.Types.EulerClient API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationResp),
    postFrfsFleetOperatorTripAction :: (API.Types.UI.FRFSTicketService.FleetOperatorTripActionReq -> EulerHS.Types.EulerClient API.Types.UI.FRFSTicketService.FleetOperatorTripActionResp)
  }

mkFrfsFleetOperatorAPIs :: (Client EulerHS.Types.EulerClient API -> FrfsFleetOperatorAPIs)
mkFrfsFleetOperatorAPIs frfsFleetOperatorClient = (FrfsFleetOperatorAPIs {..})
  where
    postFrfsFleetOperatorCurrentOperation :<|> postFrfsFleetOperatorTripAction = frfsFleetOperatorClient

data FrfsFleetOperatorUserActionType
  = POST_FRFS_FLEET_OPERATOR_CURRENT_OPERATION
  | POST_FRFS_FLEET_OPERATOR_TRIP_ACTION
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''FrfsFleetOperatorUserActionType)])
