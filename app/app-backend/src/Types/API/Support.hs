module Types.API.Support (SendIssueReq (..), SendIssueRes) where

import Beckn.Types.Common (AckResponse)
import Beckn.Types.Mobility.Issue (Issue)
import Data.Text
import EulerHS.Prelude

data SendIssueReq = SendIssueReq
  { contactEmail :: Text,
    issue :: Issue,
    productInstanceId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON)

type SendIssueRes = AckResponse
