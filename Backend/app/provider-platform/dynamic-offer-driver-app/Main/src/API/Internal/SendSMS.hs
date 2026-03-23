{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.SendSMS
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.SendSMS as Domain
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()

type API =
  ( "sendSMS"
      :> Capture "merchantShortId" Text
      :> Capture "city" Context.City
      :> Header "api-key" Text
      :> ReqBody '[JSON] Domain.SendSMSReq
      :> Post '[JSON] Domain.SendSMSRes
  )

handler :: FlowServer API
handler = sendSMS

sendSMS :: Text -> Context.City -> Maybe Text -> Domain.SendSMSReq -> FlowHandler Domain.SendSMSRes
sendSMS merchantShortId city apiKey = withFlowHandlerAPI . Domain.sendSMS apiKey merchantShortId city
