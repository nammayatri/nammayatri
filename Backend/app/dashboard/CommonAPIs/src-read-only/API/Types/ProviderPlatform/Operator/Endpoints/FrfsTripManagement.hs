{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator.Endpoints.FrfsTripManagement where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data OperatorFleetTripAction
  = TripStart
  | TripEnd
  | TripReset
  | TripRollback
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OperatorTripActionReq = OperatorTripActionReq
  { action :: OperatorFleetTripAction,
    gimsConductorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gimsDriverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets OperatorTripActionReq where
  hideSecrets = Kernel.Prelude.identity

data OperatorTripActionRes = OperatorTripActionRes {currentTripNumber :: Kernel.Prelude.Int, hasUpcomingTrips :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("operator" :> PostOperatorFrfsTripAction)

type PostOperatorFrfsTripAction = ("frfsTrip" :> "action" :> ReqBody ('[JSON]) OperatorTripActionReq :> Post ('[JSON]) OperatorTripActionRes)

newtype FrfsTripManagementAPIs = FrfsTripManagementAPIs {postOperatorFrfsTripAction :: (OperatorTripActionReq -> EulerHS.Types.EulerClient OperatorTripActionRes)}

mkFrfsTripManagementAPIs :: (Client EulerHS.Types.EulerClient API -> FrfsTripManagementAPIs)
mkFrfsTripManagementAPIs frfsTripManagementClient = (FrfsTripManagementAPIs {..})
  where
    postOperatorFrfsTripAction = frfsTripManagementClient

data FrfsTripManagementUserActionType
  = POST_OPERATOR_FRFS_TRIP_ACTION
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON FrfsTripManagementUserActionType where
  toJSON (POST_OPERATOR_FRFS_TRIP_ACTION) = Data.Aeson.String "POST_OPERATOR_FRFS_TRIP_ACTION"

instance FromJSON FrfsTripManagementUserActionType where
  parseJSON (Data.Aeson.String "POST_OPERATOR_FRFS_TRIP_ACTION") = pure POST_OPERATOR_FRFS_TRIP_ACTION
  parseJSON _ = fail "POST_OPERATOR_FRFS_TRIP_ACTION expected"

$(Data.Singletons.TH.genSingletons [(''FrfsTripManagementUserActionType)])
