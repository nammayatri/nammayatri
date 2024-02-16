{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module API.Dashboard.HotSpot
  ( API,
    handler,
    RemoveExpiredHotSpot,
  )
where

import qualified Domain.Action.UI.HotSpot as DAH
import qualified Domain.Types.Merchant as Merchant
import Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.Merchant

type API = RemoveExpiredHotSpot

type RemoveExpiredHotSpot = "removeExpired" :> Post '[JSON] APISuccess

handler :: ShortId Merchant.Merchant -> FlowServer API
handler = removeExpires

removeExpires :: ShortId Merchant.Merchant -> FlowHandler APISuccess
removeExpires merchantId = do
  m <- withFlowHandlerAPI' $ findMerchantByShortId merchantId
  withFlowHandlerAPI' $ DAH.removeExpiredHotSpots m.id
