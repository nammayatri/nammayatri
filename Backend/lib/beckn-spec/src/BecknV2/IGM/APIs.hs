{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module BecknV2.IGM.APIs where

import EulerHS.Prelude
import qualified IGM.Types as Spec
import Servant (JSON, Post, ReqBody, (:>))

type IssueAPI =
  "issue"
    :> ReqBody '[JSON] Spec.IssueReq
    :> Post '[JSON] Spec.AckResponse

issueAPI :: Proxy IssueAPI
issueAPI = Proxy

type OnIssueAPI =
  "on_issue"
    :> ReqBody '[JSON] Spec.OnIssueReq
    :> Post '[JSON] Spec.AckResponse

onIssueAPI :: Proxy OnIssueAPI
onIssueAPI = Proxy

type IssueStatusAPI =
  "issue_status"
    :> ReqBody '[JSON] Spec.IssueStatusReq
    :> Post '[JSON] Spec.AckResponse

issueStatusAPI :: Proxy IssueStatusAPI
issueStatusAPI = Proxy

type OnIssueStatusAPI =
  "on_issue_status"
    :> ReqBody '[JSON] Spec.OnIssueStatusReq
    :> Post '[JSON] Spec.AckResponse

onIssueStatusAPI :: Proxy OnIssueStatusAPI
onIssueStatusAPI = Proxy
