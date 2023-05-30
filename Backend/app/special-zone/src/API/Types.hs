{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Types where

import Data.Maybe
import Domain.Types.SpecialZone
import Kernel.External.Maps (LatLong)
import Kernel.Types.APISuccess
import Kernel.Types.App
import Kernel.Types.Id
import Servant
import Tools.Auth (DashboardTokenAuth)

---------------------------------------------------------
-- crud special zone --------------------------------------

type RegionLookupAPI =
  "lookup"
    :> "regions"
    :> MandatoryQueryParam "minLatLng" LatLong
    :> MandatoryQueryParam "maxLatLng" LatLong
    :> Get '[JSON] [SpecialZone]

type CreateSpecialZoneAPI =
  "create"
    :> ReqBody '[JSON] SpecialZoneAPIEntity
    :> Post '[JSON] APISuccess

type UpdateSpecialZoneAPI =
  "update"
    :> ReqBody '[JSON] SpecialZone
    :> Post '[JSON] APISuccess

type DeleteSpecialZoneAPI =
  "delete"
    :> MandatoryQueryParam "id" (Id SpecialZone)
    :> Delete '[JSON] APISuccess

type API = SpecialZoneAPI :<|> SpecialZoneDashboardAPIs

type SpecialZoneDashboardAPIs =
  DashboardTokenAuth
    :> "specialZone"
    :> ( RegionLookupAPI
           :<|> CreateSpecialZoneAPI
           :<|> UpdateSpecialZoneAPI
           :<|> DeleteSpecialZoneAPI
       )

type SpecialZoneAPI =
  "specialZone"
    :> "lookup"
    :> MandatoryQueryParam "latLng" LatLong
    :> Get '[JSON] (Maybe SpecialZone)
