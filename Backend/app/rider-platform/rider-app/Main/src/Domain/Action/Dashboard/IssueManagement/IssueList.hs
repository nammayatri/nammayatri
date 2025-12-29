{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.IssueManagement.IssueList
  ( getIssueListV1,
    postIssueListTicketStatusCallBack,
  )
where

import API.Types.RiderPlatform.IssueManagement.IssueList as Common (Issue (..), IssueListRes (..), Summary (..))
import qualified Data.Aeson as A
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.Dashboard.IssueManagement.Issue as ADI
import qualified Domain.Types.Issue as DI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import Environment
import qualified IssueManagement.Common as Common
import qualified IssueManagement.Domain.Action.Dashboard.Issue as DCommon
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Issue as QIssue
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.Person as QPerson

getIssueListV1 :: ShortId DM.Merchant -> Context.City -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Flow IssueListRes
getIssueListV1 merchantShortId opCity mbLimit mbOffset mbmobileCountryCode mbMobileNumber mbFrom mbTo = do
  now <- getCurrentTime
  merchant <- runInReplica $ QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let toDate = fromMaybe now mbTo
  let fromDate = fromMaybe (addUTCTime (negate nominalDay) now) mbFrom
  case mbMobileNumber of
    Just mobileNumber -> do
      mobileNumberDbHash <- getDbHash mobileNumber
      merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
      let mobileCountryCode = fromMaybe (P.getCountryMobileCode merchantOpCity.country) mbmobileCountryCode
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
      { id = cast @DI.Issue @Common.Issue issues.id,
        customerId = cast @DPerson.Person @Common.Person issues.customerId,
        bookingId = cast @DQuote.Quote @Common.Quote <$> issues.bookingId,
        firstName = person.firstName,
        lastName = person.lastName,
        mobileNumber = mobileNo,
        contactEmail = issues.contactEmail,
        reason = issues.reason,
        description = issues.description,
        createdAt = issues.createdAt,
        updatedAt = issues.updatedAt
      }

postIssueListTicketStatusCallBack :: ShortId DM.Merchant -> Context.City -> A.Value -> Flow APISuccess
postIssueListTicketStatusCallBack _ _ reqJson = do
  req <- A.decode (A.encode reqJson) & fromMaybeM (InvalidRequest "Failed to parse TicketStatusCallBackReq")
  mbTicket <- QIssue.findByTicketId req.ticketId
  case mbTicket of
    Just _ -> QIssue.updateIssueStatus req.ticketId =<< DCommon.transformKaptureStatus req
    Nothing -> void $ DCommon.ticketStatusCallBack reqJson ADI.dashboardIssueHandle Common.CUSTOMER
  return Success
