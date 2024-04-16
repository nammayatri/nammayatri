{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.IGM.Issue (buildIssueReq) where

import qualified BecknV2.IGM.Enums as Spec
import qualified BecknV2.IGM.Types as Spec
import qualified BecknV2.IGM.Utils as Utils
import qualified Data.Aeson as A
import qualified Domain.Action.Beckn.IGM.Issue as DIssue
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildIssueReq ::
  (MonadFlow m) =>
  Spec.IssueReq ->
  m DIssue.DIssue
buildIssueReq req = do
  Utils.validateContext Spec.ISSUE req.context
  issueId <- req.issueReqMessage >>= (.issueReqMessageIssue) >>= (.issueId) & fromMaybeM (InvalidRequest "IssueId not found")
  issueCategory <- req.issueReqMessage >>= (.issueReqMessageIssue) >>= (.issueCategory) & fromMaybeM (InvalidRequest "IssueCategory not found")
  issueType <- req.issueReqMessage >>= (.issueReqMessageIssue) >>= (.issueIssueType) & fromMaybeM (InvalidRequest "IssueType not found")
  issueStatus <- req.issueReqMessage >>= (.issueReqMessageIssue) >>= (.issueStatus) & fromMaybeM (InvalidRequest "IssueStatus not found")

  pure $
    DIssue.DIssue
      { issueId,
        issueCategory,
        issueType,
        issueStatus
      }
