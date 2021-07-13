module Types.API.Support
  ( module Types.API.Support,
    module Types.Issue,
  )
where

import Beckn.Types.APISuccess
import Beckn.Utils.Validation
import EulerHS.Prelude
import Types.Issue

data SendIssueReq = SendIssueReq
  { contactEmail :: Text,
    issue :: Issue,
    productInstanceId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

validateSendIssueReq :: Validate SendIssueReq
validateSendIssueReq SendIssueReq {..} =
  validateObject "issue" issue validateIssue

type SendIssueRes = APISuccess
