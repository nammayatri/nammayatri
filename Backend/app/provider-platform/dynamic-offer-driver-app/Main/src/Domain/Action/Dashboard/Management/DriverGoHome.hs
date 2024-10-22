{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Management.DriverGoHome
  ( getDriverGoHomeGetHomeLocation,
    postDriverGoHomeUpdateHomeLocation,
    postDriverGoHomeIncrementGoToCount,
    getDriverGoHomeGetGoHomeInfo,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified API.Types.ProviderPlatform.Management.DriverGoHome as Common
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id

-- TODO move handlers from Domain.Action.Dashboard.Driver
getDriverGoHomeGetHomeLocation ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow Common.GetHomeLocationsRes
getDriverGoHomeGetHomeLocation = DDriver.getDriverHomeLocation

postDriverGoHomeUpdateHomeLocation ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.UpdateDriverHomeLocationReq ->
  Flow APISuccess
postDriverGoHomeUpdateHomeLocation = DDriver.updateDriverHomeLocation

postDriverGoHomeIncrementGoToCount ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow APISuccess
postDriverGoHomeIncrementGoToCount = DDriver.incrementDriverGoToCount

getDriverGoHomeGetGoHomeInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow Common.CachedGoHomeRequestInfoRes
getDriverGoHomeGetGoHomeInfo = DDriver.getDriverGoHomeInfo
