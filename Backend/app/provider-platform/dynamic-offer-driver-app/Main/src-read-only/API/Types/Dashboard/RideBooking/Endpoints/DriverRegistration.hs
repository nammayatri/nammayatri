{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.DriverRegistration where

import qualified Dashboard.ProviderPlatform.Management.DriverRegistration
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Servant
import Servant.Client

type API = (PostDriverRegistrationAuth :<|> PostDriverRegistrationVerifyHelper)

type PostDriverRegistrationAuth =
  ( "auth" :> ReqBody '[JSON] Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq
      :> Post
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes
  )

type PostDriverRegistrationVerify =
  ( Capture "authId" Kernel.Prelude.Text :> "verify" :> ReqBody '[JSON] Dashboard.ProviderPlatform.Management.DriverRegistration.AuthVerifyReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostDriverRegistrationVerifyHelper =
  ( Capture "authId" Kernel.Prelude.Text :> Capture "mbFleet" Kernel.Prelude.Bool :> Capture "fleetOwnerId" Kernel.Prelude.Text :> "verify"
      :> ReqBody
           '[JSON]
           Dashboard.ProviderPlatform.Management.DriverRegistration.AuthVerifyReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data DriverRegistrationAPIs = DriverRegistrationAPIs
  { postDriverRegistrationAuth :: Dashboard.ProviderPlatform.Management.DriverRegistration.AuthReq -> EulerHS.Types.EulerClient Dashboard.ProviderPlatform.Management.DriverRegistration.AuthRes,
    postDriverRegistrationVerify :: Kernel.Prelude.Text -> Kernel.Prelude.Bool -> Kernel.Prelude.Text -> Dashboard.ProviderPlatform.Management.DriverRegistration.AuthVerifyReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkDriverRegistrationAPIs :: (Client EulerHS.Types.EulerClient API -> DriverRegistrationAPIs)
mkDriverRegistrationAPIs driverRegistrationClient = (DriverRegistrationAPIs {..})
  where
    postDriverRegistrationAuth :<|> postDriverRegistrationVerify = driverRegistrationClient

data DriverRegistrationUserActionType
  = POST_DRIVER_REGISTRATION_AUTH
  | POST_DRIVER_REGISTRATION_VERIFY
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''DriverRegistrationUserActionType])
