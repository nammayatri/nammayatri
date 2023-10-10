{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Domain.Action.Dashboard.IssueList where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Issue as Common
import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.Issue as DI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Issues as QIssue
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.Person as QPerson

data IssueListRes = IssueListRes
  { list :: [Issue],
    summary :: Summary
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data Issue = Issue
  { id :: Id DI.Issue,
    customerId :: Id DPerson.Person,
    bookingId :: Maybe (Id DQuote.Quote),
    firstName :: Maybe Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    contactEmail :: Maybe Text,
    reason :: Text,
    description :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data Summary = Summary
  { totalCount :: Int,
    count :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

mobileIndianCode :: Text
mobileIndianCode = "+91"

getIssueList :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Flow IssueListRes
getIssueList merchantShortId mbLimit mbOffset mbmobileCountryCode mbMobileNumber mbFrom mbTo = do
  now <- getCurrentTime
  merchant <- runInReplica $ QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let toDate = fromMaybe now mbTo
  let fromDate = fromMaybe (addUTCTime (negate nominalDay) now) mbFrom
  case mbMobileNumber of
    Just mobileNumber -> do
      mobileNumberDbHash <- getDbHash mobileNumber
      let mobileCountryCode = fromMaybe mobileIndianCode mbmobileCountryCode
      customer <- runInReplica $ QPerson.findByMobileNumberAndMerchantId mobileCountryCode mobileNumberDbHash merchant.id >>= fromMaybeM (PersonNotFound mobileNumber)
      issues <- runInReplica $ QIssue.findByCustomerId customer.id mbLimit mbOffset fromDate toDate
      issueList <- mapM buildIssueList issues
      let count = length issueList
      let summary = Summary {totalCount = count, count}
      return $ IssueListRes {list = issueList, summary = summary}
    Nothing -> do
      issues <- runInReplica $ QIssue.findAllIssue merchant.id mbLimit mbOffset fromDate toDate
      issueList <- mapM buildIssueList issues
      let count = length issueList
      let summary = Summary {totalCount = count, count}
      return $ IssueListRes {list = issueList, summary = summary}

buildIssueList :: EncFlow m r => (DI.Issue, DPerson.Person) -> m Issue
buildIssueList (issues, person) = do
  mobileNo <- mapM decrypt person.mobileNumber
  pure $
    Issue
      { id = issues.id,
        customerId = issues.customerId,
        bookingId = issues.bookingId,
        firstName = person.firstName,
        lastName = person.lastName,
        mobileNumber = mobileNo,
        contactEmail = issues.contactEmail,
        reason = issues.reason,
        description = issues.description,
        createdAt = issues.createdAt,
        updatedAt = issues.updatedAt
      }

ticketStatusCallBack :: ShortId DM.Merchant -> Common.TicketStatusCallBackReq -> Flow APISuccess
ticketStatusCallBack _ req = do
  _ <- QIssue.findByTicketId req.ticketId >>= fromMaybeM (TicketDoesNotExist req.ticketId)
  QIssue.updateIssueStatus req.ticketId (toDomainIssueStatus req.status)
  return Success

toDomainIssueStatus :: Common.IssueStatus -> DI.IssueStatus
toDomainIssueStatus = \case
  Common.OPEN -> DI.OPEN
  Common.PENDING_INTERNAL -> DI.PENDING_INTERNAL
  Common.PENDING_EXTERNAL -> DI.PENDING_EXTERNAL
  Common.RESOLVED -> DI.RESOLVED
  Common.CLOSED -> DI.CLOSED
