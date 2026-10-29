{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.SharedCab
  ( API,
    handler,
  )
where

import API.Types.UI.SharedCab
import Data.Time (Day)
import qualified Domain.Action.UI.SharedCab as DSharedCab
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

-- Driver-app /sharedCab/* API. Paths mirror the rider-app internal surface with the /internal/ prefix and driverId/
-- vehicleNumber stripped (both come from TokenAuth and the driver's Vehicle row). The booking lifecycle actions are in
-- API.UI.SharedCabBooking.

type API =
  "sharedCab"
    :> ( "routes"
           :> TokenAuth
           :> MandatoryQueryParam "lat" Double
           :> MandatoryQueryParam "lon" Double
           :> Get '[JSON] SharedCabRoutesResp
           :<|> "route"
             :> "select"
             :> TokenAuth
             :> ReqBody '[JSON] SelectRouteReq
             :> Post '[JSON] SelectRouteResp
           :<|> "session"
             :> TokenAuth
             :> Get '[JSON] (Maybe SharedCabSession)
           :<|> "seats"
             :> TokenAuth
             :> ReqBody '[JSON] SeatsReq
             :> Post '[JSON] SharedCabSession
           :<|> "route"
             :> "end"
             :> TokenAuth
             :> ReqBody '[JSON] EndRouteReq
             :> Post '[JSON] (Maybe SharedCabSession)
           :<|> "resume"
             :> TokenAuth
             :> Post '[JSON] SharedCabSession
           :<|> "trips"
             :> TokenAuth
             :> QueryParam "date" Day
             :> Get '[JSON] SharedCabTripsResp
       )

handler :: FlowServer API
handler =
  getRoutes
    :<|> selectRoute
    :<|> getSession
    :<|> setSeats
    :<|> endRoute
    :<|> resume
    :<|> getTrips

getRoutes :: (Id Person.Person, Id Merchant.Merchant, Id MerchantOperatingCity.MerchantOperatingCity) -> Double -> Double -> FlowHandler SharedCabRoutesResp
getRoutes (personId, merchantId, merchantOpCityId) lat lon = withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCab.getSharedCabRoutes (personId, merchantId, merchantOpCityId) lat lon

selectRoute :: (Id Person.Person, Id Merchant.Merchant, Id MerchantOperatingCity.MerchantOperatingCity) -> SelectRouteReq -> FlowHandler SelectRouteResp
selectRoute (personId, merchantId, merchantOpCityId) req = withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCab.selectSharedCabRoute (personId, merchantId, merchantOpCityId) req

getSession :: (Id Person.Person, Id Merchant.Merchant, Id MerchantOperatingCity.MerchantOperatingCity) -> FlowHandler (Maybe SharedCabSession)
getSession (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCab.getSharedCabSession (personId, merchantId, merchantOpCityId)

setSeats :: (Id Person.Person, Id Merchant.Merchant, Id MerchantOperatingCity.MerchantOperatingCity) -> SeatsReq -> FlowHandler SharedCabSession
setSeats (personId, merchantId, merchantOpCityId) req = withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCab.setSharedCabSeats (personId, merchantId, merchantOpCityId) req

endRoute :: (Id Person.Person, Id Merchant.Merchant, Id MerchantOperatingCity.MerchantOperatingCity) -> EndRouteReq -> FlowHandler (Maybe SharedCabSession)
endRoute (personId, merchantId, merchantOpCityId) req = withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCab.endSharedCabRoute (personId, merchantId, merchantOpCityId) req

resume :: (Id Person.Person, Id Merchant.Merchant, Id MerchantOperatingCity.MerchantOperatingCity) -> FlowHandler SharedCabSession
resume (personId, merchantId, merchantOpCityId) = withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCab.resumeSharedCab (personId, merchantId, merchantOpCityId)

getTrips :: (Id Person.Person, Id Merchant.Merchant, Id MerchantOperatingCity.MerchantOperatingCity) -> Maybe Day -> FlowHandler SharedCabTripsResp
getTrips (personId, merchantId, merchantOpCityId) mbDate = withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCab.getSharedCabTrips (personId, merchantId, merchantOpCityId) mbDate
