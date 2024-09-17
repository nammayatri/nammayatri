module IssueManagement.API.Beckn.Issue where

import qualified IssueManagement.Common.Beckn.Issue as Common
import Servant hiding (Unauthorized, throwError)

type IssueAPI =
  "beckn"
    :> Common.IssueAPI
