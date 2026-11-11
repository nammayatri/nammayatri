{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.SendWelcomeEmail
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.SendWelcomeEmail as Domain
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()

-- | Internal (api-key authed) endpoint the dashboard calls right after it
-- creates a user, to send them the Control Centre welcome email.
type API =
  "sendWelcomeEmail"
    :> Capture "merchantShortId" Text
    :> Capture "city" Context.City
    :> Header "api-key" Text
    :> ReqBody '[JSON] Domain.SendWelcomeEmailReq
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler = sendWelcomeEmail

sendWelcomeEmail :: Text -> Context.City -> Maybe Text -> Domain.SendWelcomeEmailReq -> FlowHandler APISuccess
sendWelcomeEmail merchantShortId city apiKey = withFlowHandlerAPI . Domain.sendWelcomeEmail apiKey merchantShortId city
