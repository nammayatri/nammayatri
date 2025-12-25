{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.IGM.IssueStatus (buildIssueStatusReq) where

import qualified Beckn.ACL.IGM.Utils as Utils
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import Kernel.Prelude
import Kernel.Utils.Common

buildIssueStatusReq ::
  (HasFlowEnv m r '["nwAddress" ::: BaseUrl], EncFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  DM.Merchant ->
  MerchantOperatingCity ->
  Utils.RideBooking ->
  Text ->
  Text ->
  m Spec.IssueStatusReq
buildIssueStatusReq merchant merchantOperatingCity booking becknIssueId transactionId = do
  now <- getCurrentTime
  let validTill = addUTCTime (intToNominalDiffTime 30) now
      ttl = diffUTCTime validTill now
  messageId <- generateGUID
  context <- Utils.buildContext Spec.ISSUE_STATUS Spec.ON_DEMAND merchant transactionId messageId merchantOperatingCity.city (Just $ Utils.BppData booking.providerId (showBaseUrl booking.providerUrl)) (Just $ Utils.durationToText ttl)
  pure $
    Spec.IssueStatusReq
      { issueStatusReqContext = context,
        issueStatusReqMessage = tfIssueStatusReqMessage becknIssueId
      }

tfIssueStatusReqMessage :: Text -> Spec.IssueStatusReqMessage
tfIssueStatusReqMessage becknIssueId =
  Spec.IssueStatusReqMessage
    { issueStatusReqMessageIssueId = becknIssueId
    }
