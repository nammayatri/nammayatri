{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Infobip.API.WebengageWebhook where

import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Types.APISuccess (APISuccess)
import Lib.Infobip.Types
import Servant

type ServiceAPI =
  "tracking"
    :> "privatessp-events"
    :> ReqBody '[JSON] WebengageRes
    :> Post '[JSON] APISuccess

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

sendStatus :: WebengageRes -> ET.EulerClient APISuccess
sendStatus = ET.client serviceAPI
