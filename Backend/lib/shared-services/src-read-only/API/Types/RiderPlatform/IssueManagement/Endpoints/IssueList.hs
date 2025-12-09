{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.IssueManagement.Endpoints.IssueList where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time.Calendar
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified IssueManagement.Common
import qualified IssueManagement.Domain.Types.Issue.IssueReport
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant hiding (Summary)
import Servant.Client

data DashboardIssueChat = DashboardIssueChat
  { ticketId :: Kernel.Prelude.Text,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Common.Ride),
    personId :: Kernel.Types.Id.Id IssueManagement.Common.Person,
    kaptureData :: Kernel.Prelude.Maybe Data.Aeson.Value,
    issueReportId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id IssueManagement.Domain.Types.Issue.IssueReport.IssueReport),
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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

type API = ("issue" :> (GetIssueListV1 :<|> PostIssueListTicketStatusCallBack :<|> GetIssueListChats))

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

type GetIssueListChats = ("chats" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "date" Data.Time.Calendar.Day :> Get '[JSON] [DashboardIssueChat])

data IssueListAPIs = IssueListAPIs
  { getIssueListV1 :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient IssueListRes,
    postIssueListTicketStatusCallBack :: Data.Aeson.Value -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getIssueListChats :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> EulerHS.Types.EulerClient [DashboardIssueChat]
  }

mkIssueListAPIs :: (Client EulerHS.Types.EulerClient API -> IssueListAPIs)
mkIssueListAPIs issueListClient = (IssueListAPIs {..})
  where
    getIssueListV1 :<|> postIssueListTicketStatusCallBack :<|> getIssueListChats = issueListClient

data IssueListUserActionType
  = GET_ISSUE_LIST_V1
  | POST_ISSUE_LIST_TICKET_STATUS_CALL_BACK
  | GET_ISSUE_LIST_CHATS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''IssueListUserActionType])
