{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Volunteer where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data CreateVolunteerReq = CreateVolunteerReq {place :: Kernel.Prelude.Text, vendorId :: Kernel.Prelude.Text, isActive :: Kernel.Prelude.Maybe Kernel.Prelude.Bool, personId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateVolunteerReq where
  hideSecrets = Kernel.Prelude.identity

data CreateVolunteerRes = CreateVolunteerRes {volunteerId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateVolunteerReq = UpdateVolunteerReq {isActive :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateVolunteerReq where
  hideSecrets = Kernel.Prelude.identity

data VolunteerListItem = VolunteerListItem {volunteerId :: Kernel.Prelude.Text, place :: Kernel.Prelude.Text, vendorId :: Kernel.Prelude.Text, isActive :: Kernel.Prelude.Maybe Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VolunteerListRes = VolunteerListRes {volunteers :: [VolunteerListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("volunteer" :> (PostVolunteerCreate :<|> GetVolunteerList :<|> PostVolunteerUpdate))

type PostVolunteerCreate = ("create" :> ReqBody '[JSON] CreateVolunteerReq :> Post '[JSON] CreateVolunteerRes)

type GetVolunteerList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "volunteerId" Kernel.Prelude.Text
      :> QueryParam
           "vendorId"
           Kernel.Prelude.Text
      :> QueryParam "isActive" Kernel.Prelude.Bool
      :> QueryParam "place" Kernel.Prelude.Text
      :> Get
           '[JSON]
           VolunteerListRes
  )

type PostVolunteerUpdate =
  ( Capture "volunteerId" Kernel.Prelude.Text :> Capture "vendorId" Kernel.Prelude.Text :> "update" :> ReqBody '[JSON] UpdateVolunteerReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data VolunteerAPIs = VolunteerAPIs
  { postVolunteerCreate :: CreateVolunteerReq -> EulerHS.Types.EulerClient CreateVolunteerRes,
    getVolunteerList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient VolunteerListRes,
    postVolunteerUpdate :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> UpdateVolunteerReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkVolunteerAPIs :: (Client EulerHS.Types.EulerClient API -> VolunteerAPIs)
mkVolunteerAPIs volunteerClient = (VolunteerAPIs {..})
  where
    postVolunteerCreate :<|> getVolunteerList :<|> postVolunteerUpdate = volunteerClient

data VolunteerUserActionType
  = POST_VOLUNTEER_CREATE
  | GET_VOLUNTEER_LIST
  | POST_VOLUNTEER_UPDATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''VolunteerUserActionType])
