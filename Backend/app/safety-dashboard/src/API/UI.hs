{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI
  ( API,
    handler,
  )
where

import qualified API.Action.UI.Admin as Admin
import qualified API.Action.UI.FlaggedCategory as FlaggedCategory
import qualified API.Action.UI.Merchant as Merchant
import qualified API.Action.UI.Notification as Notification
import qualified API.Action.UI.SearchSuspect as SearchSuspect
import qualified API.Action.UI.Suspect as Suspect
import qualified API.Action.UI.SuspectFlagRequest as SuspectFlagRequest
import "lib-dashboard" Environment
import EulerHS.Prelude
import Servant

type API =
  "v2"
    :> ( Get '[JSON] Text
           :<|> Suspect.API
           :<|> SuspectFlagRequest.API
           :<|> SearchSuspect.API
           :<|> Merchant.API
           :<|> FlaggedCategory.API
           :<|> Notification.API
           :<|> Admin.API
       )

handler :: FlowServer API
handler =
  pure "App is UP"
    :<|> Suspect.handler
    :<|> SuspectFlagRequest.handler
    :<|> SearchSuspect.handler
    :<|> Merchant.handler
    :<|> FlaggedCategory.handler
    :<|> Notification.handler
    :<|> Admin.handler
