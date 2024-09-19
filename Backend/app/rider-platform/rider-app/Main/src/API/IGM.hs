{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.IGM where

import qualified API.Beckn.IGM.Issue as Issue
-- import qualified API.Beckn.IGM.OnIssueStatus as OnIssueStatus
import Environment

-- type API =
--   "beckn" :> "cab" :> "v1"
--     :> SignatureAuth 'Domain.MOBILITY "Authorization"
--     :> ( OnIssue.API
--            :<|> OnIssueStatus.API
--        )

-- type APIM =
--   "beckn" :> "cab" :> "v1"
--     :> Capture "merchantId" (Id DM.Merchant)
--     :> SignatureAuth 'Domain.MOBILITY "Authorization"
--     :> ( OnIssue.API
--            :<|> OnIssueStatus.API
--        )
type IGMAPI = Issue.API

handler :: FlowServer IGMAPI
handler = Issue.handler
