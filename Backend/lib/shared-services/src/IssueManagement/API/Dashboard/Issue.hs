module IssueManagement.API.Dashboard.Issue where

import qualified IssueManagement.Common.Dashboard.Issue as Common
import Servant hiding (Unauthorized, throwError)

type DashboardIssueAPI =
  "issueV2"
    :> ( Common.IssueCategoryListAPI
           :<|> Common.IssueListAPI
           :<|> Common.IssueInfoAPI
           :<|> Common.IssueUpdateByUserAPI
           :<|> Common.IssueAddCommentByUserAPI
           :<|> Common.IssueFetchMediaAPI
           :<|> Common.TicketStatusCallBackAPI
       )
