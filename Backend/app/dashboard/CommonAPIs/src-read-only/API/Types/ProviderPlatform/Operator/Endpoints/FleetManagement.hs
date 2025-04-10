{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator.Endpoints.FleetManagement where

import qualified Dashboard.Common
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data FleetInfo = FleetInfo
  { id :: Kernel.Types.Id.Id Dashboard.Common.Person,
    name :: Kernel.Prelude.Text,
    isActive :: Kernel.Prelude.Bool,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    vehicleCount :: Kernel.Prelude.Int,
    verified :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("operator" :> GetFleetManagementFleetsHelper)

type GetFleetManagementFleets =
  ( "fleets" :> QueryParam "isActive" Kernel.Prelude.Bool :> QueryParam "verified" Kernel.Prelude.Bool :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get ('[JSON]) [FleetInfo]
  )

type GetFleetManagementFleetsHelper =
  ( "fleets" :> QueryParam "isActive" Kernel.Prelude.Bool :> QueryParam "verified" Kernel.Prelude.Bool
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Get ('[JSON]) [FleetInfo]
  )

newtype FleetManagementAPIs = FleetManagementAPIs {getFleetManagementFleets :: (Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient [FleetInfo])}

mkFleetManagementAPIs :: (Client EulerHS.Types.EulerClient API -> FleetManagementAPIs)
mkFleetManagementAPIs fleetManagementClient = (FleetManagementAPIs {..})
  where
    getFleetManagementFleets = fleetManagementClient

data FleetManagementUserActionType
  = GET_FLEET_MANAGEMENT_FLEETS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON FleetManagementUserActionType where
  toJSON (GET_FLEET_MANAGEMENT_FLEETS) = Data.Aeson.String "GET_FLEET_MANAGEMENT_FLEETS"

instance FromJSON FleetManagementUserActionType where
  parseJSON (Data.Aeson.String "GET_FLEET_MANAGEMENT_FLEETS") = pure GET_FLEET_MANAGEMENT_FLEETS
  parseJSON _ = fail "GET_FLEET_MANAGEMENT_FLEETS expected"

$(Data.Singletons.TH.genSingletons [(''FleetManagementUserActionType)])
