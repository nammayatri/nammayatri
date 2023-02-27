{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Driver (module Reexport, API, handler) where

import Domain.Action.UI.Driver as Reexport
  ( CreatePerson (..),
    CreateVehicle (..),
    DriverEntityRes (..),
    DriverInformationRes (..),
    ListDriverRes (..),
    OnboardDriverReq (..),
    OnboardDriverRes (..),
    UpdateDriverReq (..),
    UpdateDriverRes,
  )
import qualified Domain.Action.UI.Driver as DDriver
import Domain.Types.Person as SP
import Environment
import qualified Environment as App
import EulerHS.Prelude hiding (id, state)
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (withFlowHandlerAPI)
import Servant hiding (Unauthorized, throwError)
import Tools.Auth (AdminTokenAuth, TokenAuth)

type API =
  "org" :> "driver"
    :> ( AdminTokenAuth
           :> ReqBody '[JSON] OnboardDriverReq
           :> Post '[JSON] OnboardDriverRes
           :<|> "list"
             :> AdminTokenAuth
             :> QueryParam "searchString" Text
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] ListDriverRes
           :<|> AdminTokenAuth
             :> Capture "driverId" (Id Person)
             :> MandatoryQueryParam "enabled" Bool
             :> Post '[JSON] APISuccess
           :<|> AdminTokenAuth
             :> Capture "driverId" (Id Person)
             :> Delete '[JSON] APISuccess
       )
    :<|> "driver"
      :> ( "setActivity"
             :> TokenAuth
             :> MandatoryQueryParam "active" Bool
             :> Post '[JSON] APISuccess
             :<|> "setRental"
               :> TokenAuth
               :> MandatoryQueryParam "rental" Bool
               :> Post '[JSON] APISuccess
             :<|> "profile"
               :> ( TokenAuth
                      :> Get '[JSON] DriverInformationRes
                      :<|> TokenAuth
                        :> ReqBody '[JSON] UpdateDriverReq
                        :> Post '[JSON] UpdateDriverRes
                  )
         )

handler :: FlowServer API
handler =
  ( createDriver
      :<|> listDriver
      :<|> changeDriverEnableState
      :<|> deleteDriver
  )
    :<|> ( setActivity
             :<|> setRental
             :<|> ( getInformation
                      :<|> updateDriver
                  )
         )

createDriver :: SP.Person -> OnboardDriverReq -> FlowHandler OnboardDriverRes
createDriver admin = withFlowHandlerAPI . DDriver.createDriver admin

getInformation :: Id SP.Person -> App.FlowHandler DriverInformationRes
getInformation = withFlowHandlerAPI . DDriver.getInformation

setActivity :: Id SP.Person -> Bool -> App.FlowHandler APISuccess
setActivity personId = withFlowHandlerAPI . DDriver.setActivity personId

setRental :: Id SP.Person -> Bool -> App.FlowHandler APISuccess
setRental personId = withFlowHandlerAPI . DDriver.setRental personId

listDriver :: SP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler ListDriverRes
listDriver admin mbSearchString mbLimit = withFlowHandlerAPI . DDriver.listDriver admin mbSearchString mbLimit

changeDriverEnableState :: SP.Person -> Id SP.Person -> Bool -> FlowHandler APISuccess
changeDriverEnableState admin personId = withFlowHandlerAPI . DDriver.changeDriverEnableState admin personId

deleteDriver :: SP.Person -> Id SP.Person -> FlowHandler APISuccess
deleteDriver admin = withFlowHandlerAPI . DDriver.deleteDriver admin

updateDriver :: Id SP.Person -> UpdateDriverReq -> FlowHandler UpdateDriverRes
updateDriver personId = withFlowHandlerAPI . DDriver.updateDriver personId
