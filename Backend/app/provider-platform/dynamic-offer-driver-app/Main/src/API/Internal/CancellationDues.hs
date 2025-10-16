{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Internal.CancellationDues (CustomerCancellationDuesWaiveOffAPI, handler) where

import qualified Domain.Action.Internal.CancellationDues as CancellationDues
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type CustomerCancellationDuesWaiveOffAPI =
  Capture "merchantId" (Id Merchant)
    :> "customerCancellationDuesWaiveOff"
    :> Header "token" Text
    :> ReqBody '[JSON] CancellationDues.CustomerCancellationDuesWaiveOffReq
    :> Post '[JSON] APISuccess

handler :: FlowServer CustomerCancellationDuesWaiveOffAPI
handler = customerCancellationDuesWaiveOff

customerCancellationDuesWaiveOff :: Id Merchant -> Maybe Text -> CancellationDues.CustomerCancellationDuesWaiveOffReq -> FlowHandler APISuccess
customerCancellationDuesWaiveOff merchantId apiKey = withFlowHandlerAPI . CancellationDues.customerCancellationDuesWaiveOff merchantId apiKey
