{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.RideBooking.Search where

import qualified API.UI.Search as SH
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import SharedLogic.Merchant
import Storage.Beam.SystemConfigs ()
import Prelude

data RideSearchEndPoint = SearchEndPoint
  deriving (Show, Read)

derivePersistField "RideSearchEndPoint"

type API =
  "search"
    :> CustomerRideSearchAPI

type CustomerRideSearchAPI =
  Capture "customerId" (Id DP.Person)
    :> "rideSearch"
    :> ReqBody '[JSON] SH.SearchReq
    :> Post '[JSON] SH.SearchRes

handler :: ShortId DM.Merchant -> FlowServer API
handler = callSearch

callSearch :: ShortId DM.Merchant -> Id DP.Person -> SH.SearchReq -> FlowHandler SH.SearchRes
callSearch merchantId personId req = do
  m <- withFlowHandlerAPI $ findMerchantByShortId merchantId
  SH.search (personId, m.id) req Nothing Nothing Nothing
