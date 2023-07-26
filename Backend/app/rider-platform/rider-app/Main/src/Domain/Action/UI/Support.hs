{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Support
  ( SendIssueReq (..),
    SendIssueRes,
    sendIssue,
    callbackRequest,
  )
where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.CallbackRequest as DCallback
import qualified Domain.Types.Issue as DIssue
import Domain.Types.Person as Person
import Domain.Types.Quote (Quote)
import Domain.Types.Ride (Ride)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Kernel.Storage.Esqueleto (runTransaction)
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.Predicates
import Kernel.Utils.Validation
import qualified Storage.Queries.CallbackRequest as QCallback
import qualified Storage.Queries.Issues as Queries
import qualified Storage.Queries.Person as QP

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
    rideBookingId :: Maybe (Id Quote),
    rideId :: Maybe (Id Ride)
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

validateSendIssueReq :: Validate SendIssueReq
validateSendIssueReq SendIssueReq {..} =
  validateObject "issue" issue validateIssue

newtype SendIssueRes = SendIssueRes
  { issueId :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

sendIssue :: EsqDBFlow m r => Id Person.Person -> SendIssueReq -> m SendIssueRes
sendIssue personId request = do
  runRequestValidation validateSendIssueReq request
  newIssue <- buildDBIssue personId request
  runTransaction $
    Queries.insertIssue newIssue
  return $ SendIssueRes {issueId = newIssue.id.getId}

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
        updatedAt = time,
        rideId = rideId
      }

callbackRequest :: EsqDBFlow m r => Id Person.Person -> m APISuccess
callbackRequest personId = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  newCallbackRequest <- buildCallbackRequest person
  runTransaction $
    QCallback.create newCallbackRequest
  return Success

buildCallbackRequest :: MonadFlow m => Person.Person -> m DCallback.CallbackRequest
buildCallbackRequest person = do
  guid <- L.generateGUID
  now <- getCurrentTime
  customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
  customerMobileCountryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  return $
    DCallback.CallbackRequest
      { id = Id guid,
        merchantId = person.merchantId,
        customerName = person.firstName,
        customerPhone,
        customerMobileCountryCode,
        status = DCallback.PENDING,
        createdAt = now,
        updatedAt = now
      }
