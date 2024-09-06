{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.IGM.IssueStatus where

import Domain.Types.IGMConfig
import qualified Domain.Types.IGMIssue as DIGM
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity
import Environment
import qualified IGM.Enums as Spec
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeRFC339
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as QMOC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.IGMConfig as QIGMConfig
import qualified Storage.Queries.IGMIssue as QIGM

data DIssueStatus = DIssueStatus
  { issueId :: Text,
    bapId :: Text
  }
  deriving (Show, Generic, Eq)

data ValidatedDIssueStatus = ValidatedDIssueStatus
  { issue :: DIGM.IGMIssue,
    merchant :: DM.Merchant,
    igmConfig :: IGMConfig,
    merchantOperatingCity :: MerchantOperatingCity,
    bapId :: Text
  }
  deriving (Show, Generic)

data IssueStatusRes = IssueStatusRes
  { issueId :: Id DIGM.IGMIssue,
    issueStatus :: DIGM.Status,
    respondentAction :: Text,
    resolutionAction :: Maybe Text,
    groName :: Text,
    groPhone :: Text,
    groEmail :: Text,
    merchant :: DM.Merchant,
    merchantOperatingCity :: MerchantOperatingCity,
    createdAt :: UTCTimeRFC3339,
    updatedAt :: UTCTimeRFC3339,
    bapId :: Text
  }
  deriving (Show, Generic)

validateRequest :: DIssueStatus -> Flow ValidatedDIssueStatus
validateRequest DIssueStatus {..} = do
  issue <- QIGM.findByPrimaryKey (Id issueId) >>= fromMaybeM (InvalidRequest "Issue not found")
  merchant <- QM.findById (issue.merchantId) >>= fromMaybeM (InvalidRequest "Merchant not found")
  booking <- QRB.findById (issue.bookingId) >>= fromMaybeM (InvalidRequest "Booking not found")
  igmConfig <- QIGMConfig.findByMerchantId merchant.id >>= fromMaybeM (InternalError $ "IGMConfig not found " <> show merchant.id)
  merchantOperatingCity <- QMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  pure ValidatedDIssueStatus {..}

handler :: ValidatedDIssueStatus -> Flow IssueStatusRes
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
      resolutionAction = issue.resolutionAction
  pure IssueStatusRes {..}
