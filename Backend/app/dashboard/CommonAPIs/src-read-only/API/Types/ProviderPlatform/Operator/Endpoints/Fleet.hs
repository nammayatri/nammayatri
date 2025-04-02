{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Operator.Endpoints.Fleet where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import Servant
import Servant.Client

data FleetOwnerRegisterReq = FleetOwnerRegisterReq
  { city :: Kernel.Types.Beckn.Context.City,
    email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    firstName :: Kernel.Prelude.Text,
    fleetType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstCertificateImage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    operatorReferralCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panImageId1 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panImageId2 :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    panNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype FleetOwnerRegisterRes = FleetOwnerRegisterRes {personId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = (PostFleetOperatorFleetRegisterHelper :<|> PostFleetOperatorFleetLinkHelper :<|> PostFleetOperatorFleetUnlinkHelper)

type PostFleetOperatorFleetRegister = ("operator" :> "fleet" :> "register" :> ReqBody '[JSON] FleetOwnerRegisterReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostFleetOperatorFleetRegisterHelper =
  ( "operator" :> "fleet" :> "register" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> ReqBody '[JSON] FleetOwnerRegisterReq
      :> Post
           '[JSON]
           FleetOwnerRegisterRes
  )

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
  { postFleetOperatorFleetRegister :: Kernel.Prelude.Text -> FleetOwnerRegisterReq -> EulerHS.Types.EulerClient FleetOwnerRegisterRes,
    postFleetOperatorFleetLink :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postFleetOperatorFleetUnlink :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkFleetAPIs :: (Client EulerHS.Types.EulerClient API -> FleetAPIs)
mkFleetAPIs fleetClient = (FleetAPIs {..})
  where
    postFleetOperatorFleetRegister :<|> postFleetOperatorFleetLink :<|> postFleetOperatorFleetUnlink = fleetClient

data FleetUserActionType
  = POST_FLEET_OPERATOR_FLEET_REGISTER
  | POST_FLEET_OPERATOR_FLEET_LINK
  | POST_FLEET_OPERATOR_FLEET_UNLINK
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''FleetUserActionType])
