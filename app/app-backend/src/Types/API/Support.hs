module Types.API.Support (SendIssueReq (..), SendIssueRes) where

import Beckn.Types.Mobility.Issue (Issue)
import Data.Text
import EulerHS.Prelude
import Types.API.Common (Ack)

data SendIssueReq = SendIssueReq
  { contactEmail :: Text,
    issue :: Issue,
    productInstanceId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON)

type SendIssueRes = Ack
