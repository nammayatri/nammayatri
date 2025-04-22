{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator.Endpoints.Fleet where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Servant
import Servant.Client

type API = (PostFleetOperatorFleetLinkHelper :<|> PostFleetOperatorFleetUnlinkHelper)

type PostFleetOperatorFleetLink = ("operator" :> "fleet" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "link" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostFleetOperatorFleetLinkHelper =
  ( "operator" :> "fleet" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "link" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostFleetOperatorFleetUnlink = ("operator" :> "fleet" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "unlink" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostFleetOperatorFleetUnlinkHelper =
  ( "operator" :> "fleet" :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "unlink" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data FleetAPIs = FleetAPIs
  { postFleetOperatorFleetLink :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postFleetOperatorFleetUnlink :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkFleetAPIs :: (Client EulerHS.Types.EulerClient API -> FleetAPIs)
mkFleetAPIs fleetClient = (FleetAPIs {..})
  where
    postFleetOperatorFleetLink :<|> postFleetOperatorFleetUnlink = fleetClient

data FleetUserActionType
  = POST_FLEET_OPERATOR_FLEET_LINK
  | POST_FLEET_OPERATOR_FLEET_UNLINK
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''FleetUserActionType])
