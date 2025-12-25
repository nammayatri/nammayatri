{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.IssueManagement.Endpoints.IssueList where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified IssueManagement.Common
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant hiding (Summary)
import Servant.Client

data Issue = Issue
  { id :: Kernel.Types.Id.Id Issue,
    customerId :: Kernel.Types.Id.Id IssueManagement.Common.Person,
    bookingId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Common.Quote),
    firstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    contactEmail :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reason :: Kernel.Prelude.Text,
    description :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data IssueListRes = IssueListRes {list :: [Issue], summary :: Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Summary = Summary {totalCount :: Kernel.Prelude.Int, count :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("issue" :> (GetIssueListV1 :<|> PostIssueListTicketStatusCallBack))

type GetIssueListV1 =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "mobileCountryCode" Kernel.Prelude.Text
      :> QueryParam
           "mobileNumber"
           Kernel.Prelude.Text
      :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           IssueListRes
  )

type PostIssueListTicketStatusCallBack = ("kapture" :> "ticketStatus" :> ReqBody '[JSON] Data.Aeson.Value :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data IssueListAPIs = IssueListAPIs
  { getIssueListV1 :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient IssueListRes,
    postIssueListTicketStatusCallBack :: Data.Aeson.Value -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkIssueListAPIs :: (Client EulerHS.Types.EulerClient API -> IssueListAPIs)
mkIssueListAPIs issueListClient = (IssueListAPIs {..})
  where
    getIssueListV1 :<|> postIssueListTicketStatusCallBack = issueListClient

data IssueListUserActionType
  = GET_ISSUE_LIST_V1
  | POST_ISSUE_LIST_TICKET_STATUS_CALL_BACK
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''IssueListUserActionType])
