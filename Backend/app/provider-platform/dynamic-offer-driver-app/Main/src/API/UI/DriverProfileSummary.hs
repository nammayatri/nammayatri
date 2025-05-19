{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.DriverProfileSummary where

import qualified Domain.Action.UI.DriverProfileSummary as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "driver"
    :> "profile"
    :> ( "summary"
           :> TokenAuth
           :> QueryParam "fleetInfo" Bool
           :> Get '[JSON] Domain.DriverProfleSummaryRes
       )

handler :: FlowServer API
handler =
  getDriverProfileSummary

getDriverProfileSummary :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Bool -> FlowHandler Domain.DriverProfleSummaryRes
getDriverProfileSummary (driverId, merchantId, merchantOpCityId) mbFleetInfo = withFlowHandlerAPI $ Domain.getDriverProfileSummary (driverId, merchantId, merchantOpCityId) mbFleetInfo
