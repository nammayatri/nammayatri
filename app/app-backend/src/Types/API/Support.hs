module Types.API.Support (SendIssueReq (..), SendIssueRes) where

import Beckn.Types.APISuccess
import EulerHS.Prelude
import Types.Issue (Issue)

data SendIssueReq = SendIssueReq
  { contactEmail :: Text,
    issue :: Issue,
    productInstanceId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type SendIssueRes = APISuccess
