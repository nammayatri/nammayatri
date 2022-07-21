module API.UI.Driver (module Reexport, API, handler) where

import App.Types
import qualified App.Types as App
import Beckn.Types.APISuccess
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
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
import EulerHS.Prelude hiding (id, state)
import Servant hiding (Unauthorized, throwError)
import Utils.Auth (AdminTokenAuth, TokenAuth)
import Utils.Common (withFlowHandlerAPI)

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

setActivity :: Id SP.Person -> Bool -> App.FlowHandler APISuccess.APISuccess
setActivity personId = withFlowHandlerAPI . DDriver.setActivity personId

setRental :: Id SP.Person -> Bool -> App.FlowHandler APISuccess.APISuccess
setRental personId = withFlowHandlerAPI . DDriver.setRental personId

listDriver :: SP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler ListDriverRes
listDriver admin mbSearchString mbLimit = withFlowHandlerAPI . DDriver.listDriver admin mbSearchString mbLimit

changeDriverEnableState :: SP.Person -> Id SP.Person -> Bool -> FlowHandler APISuccess
changeDriverEnableState admin personId = withFlowHandlerAPI . DDriver.changeDriverEnableState admin personId

deleteDriver :: SP.Person -> Id SP.Person -> FlowHandler APISuccess
deleteDriver admin = withFlowHandlerAPI . DDriver.deleteDriver admin

updateDriver :: Id SP.Person -> UpdateDriverReq -> FlowHandler UpdateDriverRes
updateDriver personId = withFlowHandlerAPI . DDriver.updateDriver personId
