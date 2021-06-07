module Types.API.Support (SendIssueReq (..), SendIssueRes) where

import EulerHS.Prelude
import Types.API.Common (Ack)
import Types.Issue (Issue)

data SendIssueReq = SendIssueReq
  { contactEmail :: Text,
    issue :: Issue,
    productInstanceId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON)

type SendIssueRes = Ack
