{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.IssueList where

import qualified Domain.Action.Dashboard.IssueList as DDI
import qualified Domain.Action.Dashboard.IssueList as DI
import qualified Domain.Types.Merchant as DM
import Environment
import qualified IssueManagement.Common.Dashboard.Issue as Common
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  "issue"
    :> ( ListCustomerIssue
           :<|> TicketStatusCallBack
       )

type ListCustomerIssue =
  "list"
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "mobileCountryCode" Text
    :> QueryParam "mobileNumber" Text
    :> QueryParam "from" UTCTime
    :> QueryParam "to" UTCTime
    :> Get '[JSON] DI.IssueListRes

type TicketStatusCallBack = Common.TicketStatusCallBackAPI

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantShortId =
  listIssue merchantShortId
    :<|> ticketStatusCallBack merchantShortId

listIssue :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> FlowHandler DI.IssueListRes
listIssue merchantShortId mbLimit mbOffset mbMobileCountryCode mbMobileNumber mbFrom mbTo = withFlowHandlerAPI $ DDI.getIssueList merchantShortId mbLimit mbOffset mbMobileCountryCode mbMobileNumber mbFrom mbTo

ticketStatusCallBack :: ShortId DM.Merchant -> Common.TicketStatusCallBackReq -> FlowHandler APISuccess
ticketStatusCallBack merchantShortId = withFlowHandlerAPI . DDI.ticketStatusCallBack merchantShortId
