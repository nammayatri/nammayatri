{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Serviceability
  ( API,
    handler,
    ServiceabilityReq (..),
    ServiceabilityRes (..),
  )
where

import qualified Domain.Action.UI.Serviceability as DServiceability
import Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Types.CommonImport
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

-------- Serviceability----------
type API =
  "serviceability"
    :> TokenAuth
    :> ( "origin"
           :> ReqBody '[JSON] ServiceabilityReq
           :> Post '[JSON] ServiceabilityRes
           :<|> "destination"
             :> ReqBody '[JSON] ServiceabilityReq
             :> Post '[JSON] ServiceabilityRes
       )

newtype ServiceabilityReq = ServiceabilityReq
  { location :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype ServiceabilityRes = ServiceabilityRes
  { serviceable :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

handler :: FlowServer API
handler regToken =
  checkServiceability origin regToken
    :<|> checkServiceability destination regToken

checkServiceability ::
  (GeofencingConfig -> GeoRestriction) ->
  Id Person.Person ->
  ServiceabilityReq ->
  FlowHandler ServiceabilityRes
checkServiceability settingAccessor personId ServiceabilityReq {..} = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  ServiceabilityRes <$> DServiceability.checkServiceability settingAccessor personId location
