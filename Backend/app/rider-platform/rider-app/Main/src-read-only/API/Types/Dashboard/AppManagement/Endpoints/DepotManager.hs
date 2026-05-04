{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.DepotManager where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data DepotManagerDetail = DepotManagerDetail {countryCode :: Kernel.Prelude.Text, depotCode :: Kernel.Prelude.Text, isAdmin :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, mobileNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DepotManagerDetail where
  hideSecrets = Kernel.Prelude.identity

data DepotManagerDetails = DepotManagerDetails {items :: [DepotManagerDetail]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DepotManagerDetails where
  hideSecrets = Kernel.Prelude.identity

data UpsertDepotManagerResp = UpsertDepotManagerResp
  { failedCount :: Kernel.Prelude.Int,
    failedPhoneNumbers :: [Kernel.Prelude.Text],
    succeededCount :: Kernel.Prelude.Int,
    succeededPhoneNumbers :: [Kernel.Prelude.Text],
    success :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("depotManager" :> (PostDepotManagerUpsertOne :<|> PostDepotManagerUpsertMany))

type PostDepotManagerUpsertOne = ("upsertOne" :> ReqBody '[JSON] DepotManagerDetail :> Post '[JSON] UpsertDepotManagerResp)

type PostDepotManagerUpsertMany = ("upsertMany" :> ReqBody '[JSON] DepotManagerDetails :> Post '[JSON] UpsertDepotManagerResp)

data DepotManagerAPIs = DepotManagerAPIs
  { postDepotManagerUpsertOne :: DepotManagerDetail -> EulerHS.Types.EulerClient UpsertDepotManagerResp,
    postDepotManagerUpsertMany :: DepotManagerDetails -> EulerHS.Types.EulerClient UpsertDepotManagerResp
  }

mkDepotManagerAPIs :: (Client EulerHS.Types.EulerClient API -> DepotManagerAPIs)
mkDepotManagerAPIs depotManagerClient = (DepotManagerAPIs {..})
  where
    postDepotManagerUpsertOne :<|> postDepotManagerUpsertMany = depotManagerClient

data DepotManagerUserActionType
  = POST_DEPOT_MANAGER_UPSERT_ONE
  | POST_DEPOT_MANAGER_UPSERT_MANY
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''DepotManagerUserActionType])
