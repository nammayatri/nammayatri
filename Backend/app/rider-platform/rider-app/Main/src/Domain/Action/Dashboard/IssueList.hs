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

import qualified Domain.Types.Issue as DI
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Issues as QIssue
import qualified Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.Person as QPerson

newtype IssueListRes = IssueListRes
  { list :: [DI.Issue]
  }

getIssueList :: ShortId DM.Merchant -> Text -> Text -> Flow [DI.Issue]
getIssueList merchantShortId mobileCountryCode mobileNumber = do
  mobileNumberDbHash <- getDbHash mobileNumber
  merchant <- runInReplica $ QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  customer <- runInReplica $ QPerson.findByMobileNumberAndMerchantId mobileCountryCode mobileNumberDbHash merchant.id >>= fromMaybeM (PersonNotFound mobileNumber)
  runInReplica $ QIssue.findByCustomerId customer.id
