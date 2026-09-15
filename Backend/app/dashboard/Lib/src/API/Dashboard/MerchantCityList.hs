{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Each merchant with its supported operating cities, for assigning a user
-- merchant/city access. Unrelated to permissions -- it keeps the
-- @\/admin\/accessMatrix@ path prefix only so the path is unchanged.
module API.Dashboard.MerchantCityList
  ( API,
    handler,
  )
where

import qualified Domain.Action.Dashboard.AccessMatrix as DAccessMatrix
import qualified Domain.Types.MerchantCityList as DMatrix
import Kernel.Types.Flow (FlowR)
import Kernel.Utils.Common
import Servant
import Tools.Auth.DashboardLoginFlow (DashboardLoginFlow)

type API =
  "admin"
    :> "accessMatrix"
    :> "merchantWithCityList"
    :> Get '[JSON] [DMatrix.MerchantCityList]

handler :: DashboardLoginFlow (FlowR r) r => FlowServerR r API
handler = getMerchantWithCityList

getMerchantWithCityList :: DashboardLoginFlow (FlowR r) r => FlowHandlerR r [DMatrix.MerchantCityList]
getMerchantWithCityList =
  withFlowHandlerAPI' DAccessMatrix.getMerchantWithCityList
