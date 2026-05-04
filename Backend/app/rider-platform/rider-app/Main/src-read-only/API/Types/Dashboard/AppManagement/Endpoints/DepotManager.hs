{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.DepotManager where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data DeleteDepotManagerReq = DeleteDepotManagerReq {depotCode :: Kernel.Prelude.Text, personId :: Kernel.Types.Id.Id Domain.Types.Person.Person}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DeleteDepotManagerReq where
  hideSecrets = Kernel.Prelude.identity

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

data DepotManagerInfo = DepotManagerInfo
  { countryCode :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    depotCode :: Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    isAdmin :: Kernel.Prelude.Bool,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DepotManagerListResp = DepotManagerListResp {items :: [DepotManagerInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpsertDepotManagerResp = UpsertDepotManagerResp
  { failedCount :: Kernel.Prelude.Int,
    failedPhoneNumbers :: [Kernel.Prelude.Text],
    succeededCount :: Kernel.Prelude.Int,
    succeededPhoneNumbers :: [Kernel.Prelude.Text],
    success :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("depotManager" :> (PostDepotManagerUpsertOne :<|> PostDepotManagerUpsertMany :<|> GetDepotManagerList :<|> DeleteDepotManager :<|> GetDepotManagerByMobileNumber :<|> GetDepotManagerByDepotCode :<|> GetDepotManagerByPersonId))

type PostDepotManagerUpsertOne = ("upsertOne" :> ReqBody ('[JSON]) DepotManagerDetail :> Post ('[JSON]) UpsertDepotManagerResp)

type PostDepotManagerUpsertMany = ("upsertMany" :> ReqBody ('[JSON]) DepotManagerDetails :> Post ('[JSON]) UpsertDepotManagerResp)

type GetDepotManagerList = ("list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> Get ('[JSON]) DepotManagerListResp)

type DeleteDepotManager = ("delete" :> ReqBody ('[JSON]) DeleteDepotManagerReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type GetDepotManagerByMobileNumber = ("findByMobileNumber" :> QueryParam "mobileNumber" Kernel.Prelude.Text :> QueryParam "countryCode" Kernel.Prelude.Text :> Get ('[JSON]) DepotManagerInfo)

type GetDepotManagerByDepotCode =
  ( "findByDepotCode" :> QueryParam "depotCode" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> Get
           ('[JSON])
           DepotManagerListResp
  )

type GetDepotManagerByPersonId = ("findByPersonId" :> QueryParam "personId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> Get ('[JSON]) DepotManagerInfo)

data DepotManagerAPIs = DepotManagerAPIs
  { postDepotManagerUpsertOne :: (DepotManagerDetail -> EulerHS.Types.EulerClient UpsertDepotManagerResp),
    postDepotManagerUpsertMany :: (DepotManagerDetails -> EulerHS.Types.EulerClient UpsertDepotManagerResp),
    getDepotManagerList :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient DepotManagerListResp),
    deleteDepotManager :: (DeleteDepotManagerReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    getDepotManagerByMobileNumber :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient DepotManagerInfo),
    getDepotManagerByDepotCode :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> EulerHS.Types.EulerClient DepotManagerListResp),
    getDepotManagerByPersonId :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> EulerHS.Types.EulerClient DepotManagerInfo)
  }

mkDepotManagerAPIs :: (Client EulerHS.Types.EulerClient API -> DepotManagerAPIs)
mkDepotManagerAPIs depotManagerClient = (DepotManagerAPIs {..})
  where
    postDepotManagerUpsertOne :<|> postDepotManagerUpsertMany :<|> getDepotManagerList :<|> deleteDepotManager :<|> getDepotManagerByMobileNumber :<|> getDepotManagerByDepotCode :<|> getDepotManagerByPersonId = depotManagerClient

data DepotManagerUserActionType
  = POST_DEPOT_MANAGER_UPSERT_ONE
  | POST_DEPOT_MANAGER_UPSERT_MANY
  | GET_DEPOT_MANAGER_LIST
  | DELETE_DEPOT_MANAGER
  | GET_DEPOT_MANAGER_BY_MOBILE_NUMBER
  | GET_DEPOT_MANAGER_BY_DEPOT_CODE
  | GET_DEPOT_MANAGER_BY_PERSON_ID
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''DepotManagerUserActionType)])
