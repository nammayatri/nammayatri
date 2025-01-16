{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module IssueManagement.Domain.Action.Beckn.IssueStatus where

import qualified IGM.Enums as Spec
import IssueManagement.Common
import IssueManagement.Domain.Action.UI.Issue
import IssueManagement.Domain.Types.Issue.IGMConfig
import qualified IssueManagement.Domain.Types.Issue.IGMIssue as DIGM
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IGMConfig as QIGMConfig
import qualified IssueManagement.Storage.Queries.Issue.IGMIssue as QIGM
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common

data DIssueStatus = DIssueStatus
  { issueId :: Text,
    bapId :: Text
  }
  deriving (Show, Generic, Eq)

data ValidatedDIssueStatus = ValidatedDIssueStatus
  { issue :: DIGM.IGMIssue,
    merchant :: Merchant,
    igmConfig :: IGMConfig,
    merchantOperatingCity :: MerchantOperatingCity,
    bapId :: Text
  }
  deriving (Show, Generic)

data IssueStatusRes = IssueStatusRes
  { issueId :: Id DIGM.IGMIssue,
    issueStatus :: DIGM.Status,
    respondentAction :: Text,
    groName :: Text,
    groPhone :: Text,
    groEmail :: Text,
    merchant :: Merchant,
    merchantOperatingCity :: MerchantOperatingCity,
    createdAt :: UTCTimeRFC3339,
    updatedAt :: UTCTimeRFC3339,
    bapId :: Text
  }
  deriving (Show, Generic)

validateRequest ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  DIssueStatus ->
  ServiceHandle m ->
  m ValidatedDIssueStatus
validateRequest DIssueStatus {..} iHandle = do
  issue <- QIGM.findByPrimaryKey (Id issueId) >>= fromMaybeM (InvalidRequest "Issue not found")
  booking <- iHandle.findByBookingId (Id issue.bookingId) >>= fromMaybeM (BookingDoesNotExist issue.bookingId)
  let merchantId = fromMaybe booking.providerId issue.merchantId
  merchant <- iHandle.findByMerchantId merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  igmConfig <- QIGMConfig.findByMerchantId merchantId >>= fromMaybeM (InternalError $ "IGMConfig not found " <> show merchantId)
  merchantOperatingCity <- iHandle.findMOCityById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  pure ValidatedDIssueStatus {..}

handler ::
  ( BeamFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  ValidatedDIssueStatus ->
  m IssueStatusRes
handler ValidatedDIssueStatus {..} = do
  now <- getCurrentTime
  let issueStatus = issue.issueStatus
      issueId = issue.id
      respondentAction = fromMaybe (show Spec.PROCESSING) issue.respondentAction
      groName = igmConfig.groName
      groPhone = igmConfig.groPhone
      groEmail = igmConfig.groEmail
      createdAt = UTCTimeRFC3339 issue.createdAt
      updatedAt = UTCTimeRFC3339 now
  pure IssueStatusRes {..}
