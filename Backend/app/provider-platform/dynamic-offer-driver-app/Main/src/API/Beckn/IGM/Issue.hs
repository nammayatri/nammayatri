{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.IGM.Issue where

import qualified API.UI.Issue as AUI
import Environment
import qualified IGM.Types as Spec
import qualified IssueManagement.API.Beckn.Issue as BI
import qualified IssueManagement.Common as Common
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant

type API = BI.IssueAPI

handler :: FlowServer API
handler = onDemandHandler :<|> publicTransportHandler
  where
    onDemandHandler = onDemandBAPHandler :<|> onDemandBPPHandler

    onDemandBAPHandler merchantId sigAuth =
      onIssue merchantId sigAuth :<|> onIssueStatus merchantId sigAuth

    onDemandBPPHandler merchantId sigAuth =
      issue merchantId sigAuth :<|> issueStatus merchantId sigAuth

    publicTransportHandler merchantId sigAuth =
      onIssue merchantId sigAuth :<|> onIssueStatus merchantId sigAuth

issue :: Id Common.Merchant -> SignatureAuthResult -> Spec.IssueReq -> FlowHandler Spec.AckResponse
issue merchantId _ issueReq = withFlowHandlerAPI $ BI.issue (cast merchantId) issueReq AUI.driverIssueHandle Common.DRIVER

onIssue :: Id Common.Merchant -> SignatureAuthResult -> Spec.OnIssueReq -> FlowHandler Spec.AckResponse
onIssue merchantId _ onIssueReq = withFlowHandlerAPI $ BI.onIssue (cast merchantId) onIssueReq AUI.driverIssueHandle Common.DRIVER

issueStatus :: Id Common.Merchant -> SignatureAuthResult -> Spec.IssueStatusReq -> FlowHandler Spec.AckResponse
issueStatus merchantId _ issueStatusReq = withFlowHandlerAPI $ BI.issueStatus (cast merchantId) issueStatusReq AUI.driverIssueHandle Common.DRIVER

onIssueStatus :: Id Common.Merchant -> SignatureAuthResult -> Spec.OnIssueStatusReq -> FlowHandler Spec.AckResponse
onIssueStatus merchantId _ onIssueStatusReq = withFlowHandlerAPI $ BI.onIssueStatus (cast merchantId) onIssueStatusReq AUI.driverIssueHandle Common.DRIVER
