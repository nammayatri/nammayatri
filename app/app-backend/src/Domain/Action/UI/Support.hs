module Domain.Action.UI.Support
  ( SendIssueReq (..),
    SendIssueRes,
    sendIssue,
  )
where

import Beckn.Storage.Esqueleto (runTransaction)
import Beckn.Types.APISuccess
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Predicate
import Beckn.Utils.Predicates
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Issue as DIssue
import Domain.Types.Person as Person
import Domain.Types.Quote (Quote)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Issues as Queries

data Issue = Issue
  { reason :: Text,
    description :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

validateIssue :: Validate Issue
validateIssue Issue {..} =
  sequenceA_
    [ validateField "reason" reason $ LengthInRange 2 500 `And` text,
      validateField "description" description $ LengthInRange 2 1000 `And` text
    ]
  where
    text = star $ alphanum \/ " " \/ ","

data SendIssueReq = SendIssueReq
  { contactEmail :: Maybe Text,
    issue :: Issue,
    rideBookingId :: Maybe (Id Quote)
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

validateSendIssueReq :: Validate SendIssueReq
validateSendIssueReq SendIssueReq {..} =
  validateObject "issue" issue validateIssue

type SendIssueRes = APISuccess

sendIssue :: EsqDBFlow m r => Id Person.Person -> SendIssueReq -> m SendIssueRes
sendIssue personId request@SendIssueReq {..} = do
  runRequestValidation validateSendIssueReq request
  newIssue <- buildDBIssue personId request
  runTransaction $
    Queries.insertIssue newIssue
  return APISuccess.Success

buildDBIssue :: MonadFlow m => Id Person.Person -> SendIssueReq -> m DIssue.Issue
buildDBIssue (Id customerId) SendIssueReq {..} = do
  issueId <- L.generateGUID
  time <- getCurrentTime
  return $
    DIssue.Issue
      { id = Id issueId,
        customerId = Id customerId,
        bookingId = rideBookingId,
        contactEmail = contactEmail,
        reason = issue.reason,
        description = issue.description,
        createdAt = time,
        updatedAt = time
      }
