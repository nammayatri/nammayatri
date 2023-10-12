{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

-}

module API.Dashboard.Revenue where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Revenue as Common
import Data.Maybe
import Data.Text
import Data.Time
import qualified Domain.Action.Dashboard.Revenue as DRevenue
import qualified Domain.Types.Merchant as DM
import Environment
import GHC.Base
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant

type API =
  "revenue"
    :> Common.GetCollectionHistory
    :<|> Common.GetAllDriverFeeHistory

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  getCollectionHistory merchantId
    :<|> getAllDriverFeeHistory merchantId

getCollectionHistory :: ShortId DM.Merchant -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> FlowHandler Common.CollectionList
getCollectionHistory merchantShortId volunteerId place mbFrom mbTo = withFlowHandlerAPI $ DRevenue.getCollectionHistory merchantShortId volunteerId place mbFrom mbTo

getAllDriverFeeHistory :: ShortId DM.Merchant -> Maybe UTCTime -> Maybe UTCTime -> FlowHandler [Common.AllFees]
getAllDriverFeeHistory merchantShortId mbFrom mbTo = withFlowHandlerAPI $ DRevenue.getAllDriverFeeHistory merchantShortId mbFrom mbTo
