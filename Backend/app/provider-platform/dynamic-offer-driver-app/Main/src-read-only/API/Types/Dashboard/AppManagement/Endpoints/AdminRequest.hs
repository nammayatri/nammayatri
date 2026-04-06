{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.AdminRequest where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Types.AdminRequest
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data AdminRequestItem = AdminRequestItem
  { adminRequestId :: Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    actionType :: Domain.Types.AdminRequest.ActionType,
    adjustmentType :: Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentType,
    referenceType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referenceId :: Kernel.Prelude.Text,
    referenceTable :: Domain.Types.AdminRequest.ReferenceTable,
    amount :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    source :: Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentSource,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    adminMakerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    adminCheckerId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    status :: Domain.Types.AdminRequest.AdminRequestStatus,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    adminMakerName :: Kernel.Prelude.Text,
    adminCheckerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AdminRequestResp = AdminRequestResp {summary :: Dashboard.Common.Summary, adminRequests :: [AdminRequestItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateAdminRequestReq = CreateAdminRequestReq
  { personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    actionType :: Domain.Types.AdminRequest.ActionType,
    adjustmentType :: Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentType,
    referenceType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referenceId :: Kernel.Prelude.Text,
    referenceTable :: Domain.Types.AdminRequest.ReferenceTable,
    amount :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
    source :: Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentSource,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateAdminRequestReq where
  hideSecrets = Kernel.Prelude.identity

newtype RespondAdminRequestReq = RespondAdminRequestReq {approve :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets RespondAdminRequestReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("adminRequest" :> (PostAdminRequestCreateHelper :<|> GetAdminRequestListHelper :<|> PostAdminRequestRespondHelper))

type PostAdminRequestCreate = ("create" :> ReqBody '[JSON] CreateAdminRequestReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostAdminRequestCreateHelper =
  ( "create" :> MandatoryQueryParam "requestorId" Kernel.Prelude.Text :> MandatoryQueryParam "requestorName" Kernel.Prelude.Text
      :> ReqBody
           '[JSON]
           CreateAdminRequestReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type GetAdminRequestList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "adminRequestId"
           (Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest)
      :> QueryParam "status" Domain.Types.AdminRequest.AdminRequestStatus
      :> QueryParam
           "personId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> QueryParam
           "excludeCurrentAdminMaker"
           Kernel.Prelude.Bool
      :> QueryParam
           "actionType"
           Domain.Types.AdminRequest.ActionType
      :> QueryParam
           "adjustmentType"
           Domain.Types.AdminRequest.AdjustmentType
      :> QueryParam
           "referenceType"
           Kernel.Prelude.Text
      :> QueryParam
           "referenceId"
           Kernel.Prelude.Text
      :> QueryParam
           "referenceTable"
           Domain.Types.AdminRequest.ReferenceTable
      :> QueryParam
           "source"
           Domain.Types.AdminRequest.AdjustmentSource
      :> QueryParam
           "adminMakerId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> QueryParam
           "adminCheckerId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           AdminRequestResp
  )

type GetAdminRequestListHelper =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "adminRequestId"
           (Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest)
      :> QueryParam "status" Domain.Types.AdminRequest.AdminRequestStatus
      :> QueryParam
           "personId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> QueryParam
           "excludeCurrentAdminMaker"
           Kernel.Prelude.Bool
      :> QueryParam
           "actionType"
           Domain.Types.AdminRequest.ActionType
      :> QueryParam
           "adjustmentType"
           Domain.Types.AdminRequest.AdjustmentType
      :> QueryParam
           "referenceType"
           Kernel.Prelude.Text
      :> QueryParam
           "referenceId"
           Kernel.Prelude.Text
      :> QueryParam
           "referenceTable"
           Domain.Types.AdminRequest.ReferenceTable
      :> QueryParam
           "source"
           Domain.Types.AdminRequest.AdjustmentSource
      :> QueryParam
           "adminMakerId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> QueryParam
           "adminCheckerId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> MandatoryQueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           AdminRequestResp
  )

type PostAdminRequestRespond =
  ( Capture "adminRequestId" (Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest) :> "respond" :> ReqBody '[JSON] RespondAdminRequestReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

type PostAdminRequestRespondHelper =
  ( Capture "adminRequestId" (Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest) :> "respond"
      :> MandatoryQueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "requestorName" Kernel.Prelude.Text
      :> ReqBody '[JSON] RespondAdminRequestReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data AdminRequestAPIs = AdminRequestAPIs
  { postAdminRequestCreate :: Kernel.Prelude.Text -> Kernel.Prelude.Text -> CreateAdminRequestReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getAdminRequestList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest) -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdminRequestStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.ActionType -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.ReferenceTable -> Kernel.Prelude.Maybe Domain.Types.AdminRequest.AdjustmentSource -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient AdminRequestResp,
    postAdminRequestRespond :: Kernel.Types.Id.Id Domain.Types.AdminRequest.AdminRequest -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> RespondAdminRequestReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkAdminRequestAPIs :: (Client EulerHS.Types.EulerClient API -> AdminRequestAPIs)
mkAdminRequestAPIs adminRequestClient = (AdminRequestAPIs {..})
  where
    postAdminRequestCreate :<|> getAdminRequestList :<|> postAdminRequestRespond = adminRequestClient

data AdminRequestUserActionType
  = POST_ADMIN_REQUEST_CREATE
  | GET_ADMIN_REQUEST_LIST
  | POST_ADMIN_REQUEST_RESPOND
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''AdminRequestUserActionType])
