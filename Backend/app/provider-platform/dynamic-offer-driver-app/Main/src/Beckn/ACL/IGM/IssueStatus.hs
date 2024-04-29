{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.IGM.IssueStatus (buildIssueStatusReq) where

import qualified Domain.Action.Beckn.IGM.IssueStatus as DIssueStatus
import qualified IGM.Enums as Spec
import qualified IGM.Types as Spec
import qualified IGM.Utils as Utils
import Kernel.Prelude
import Kernel.Utils.Common

buildIssueStatusReq ::
  (MonadFlow m) =>
  Spec.IssueStatusReq ->
  m DIssueStatus.DIssueStatus
buildIssueStatusReq req = do
  Utils.validateContext Spec.ON_ISSUE req.issueStatusReqContext
  let issueId = req.issueStatusReqMessage.issueStatusReqMessageIssueId
  pure $
    DIssueStatus.DIssueStatus
      { issueId = issueId
      }
