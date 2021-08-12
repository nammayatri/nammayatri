module Types.API.Support
  ( module Types.API.Support,
    module Types.Issue,
  )
where

import Beckn.Types.APISuccess
import Beckn.Types.Id
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Types.Issue
import Types.Storage.ProductInstance (ProductInstance)

data SendIssueReq = SendIssueReq
  { contactEmail :: Maybe Text,
    issue :: Issue,
    rideBookingId :: Maybe (Id ProductInstance)
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

validateSendIssueReq :: Validate SendIssueReq
validateSendIssueReq SendIssueReq {..} =
  validateObject "issue" issue validateIssue

type SendIssueRes = APISuccess
