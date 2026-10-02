{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.FrfsFleetOperator where

import qualified "this" API.Types.UI.FRFSFleetOperator
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import Servant
import Servant.Client

type API = ("FrfsFleetOperator" :> (PostFrfsFleetOperatorCurrentOperation :<|> PostFrfsFleetOperatorTripAction :<|> PostFrfsFleetOperatorV2CurrentOperationHelper :<|> PostFrfsFleetOperatorV2TripActionHelper))

type PostFrfsFleetOperatorCurrentOperation =
  ( "currentOperation" :> ReqBody ('[JSON]) API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq
      :> Post
           ('[JSON])
           API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp
  )

type PostFrfsFleetOperatorTripAction =
  ( "tripAction" :> ReqBody ('[JSON]) API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq
      :> Post
           ('[JSON])
           API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp
  )

type PostFrfsFleetOperatorV2CurrentOperation =
  ( "v2" :> "currentOperation" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Req
      :> Post ('[JSON]) API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp
  )

type PostFrfsFleetOperatorV2CurrentOperationHelper =
  ( "v2" :> "currentOperation" :> QueryParam "operatorId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Req
      :> Post
           ('[JSON])
           API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp
  )

type PostFrfsFleetOperatorV2TripAction =
  ( "v2" :> "tripAction" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionV2Req
      :> Post ('[JSON]) API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp
  )

type PostFrfsFleetOperatorV2TripActionHelper =
  ( "v2" :> "tripAction" :> QueryParam "operatorId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionV2Req
      :> Post
           ('[JSON])
           API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp
  )

data FrfsFleetOperatorAPIs = FrfsFleetOperatorAPIs
  { postFrfsFleetOperatorCurrentOperation :: (API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq -> EulerHS.Types.EulerClient API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp),
    postFrfsFleetOperatorTripAction :: (API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq -> EulerHS.Types.EulerClient API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp),
    postFrfsFleetOperatorV2CurrentOperation :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Req -> EulerHS.Types.EulerClient API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp),
    postFrfsFleetOperatorV2TripAction :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionV2Req -> EulerHS.Types.EulerClient API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp)
  }

mkFrfsFleetOperatorAPIs :: (Client EulerHS.Types.EulerClient API -> FrfsFleetOperatorAPIs)
mkFrfsFleetOperatorAPIs frfsFleetOperatorClient = (FrfsFleetOperatorAPIs {..})
  where
    postFrfsFleetOperatorCurrentOperation :<|> postFrfsFleetOperatorTripAction :<|> postFrfsFleetOperatorV2CurrentOperation :<|> postFrfsFleetOperatorV2TripAction = frfsFleetOperatorClient

data FrfsFleetOperatorUserActionType
  = POST_FRFS_FLEET_OPERATOR_CURRENT_OPERATION
  | POST_FRFS_FLEET_OPERATOR_TRIP_ACTION
  | POST_FRFS_FLEET_OPERATOR_V2_CURRENT_OPERATION
  | POST_FRFS_FLEET_OPERATOR_V2_TRIP_ACTION
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''FrfsFleetOperatorUserActionType)])
