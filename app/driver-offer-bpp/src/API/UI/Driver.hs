module API.UI.Driver
  ( DDriver.DriverInformationRes (..),
    DDriver.ListDriverRes (..),
    DDriver.DriverEntityRes (..),
    DDriver.OnboardDriverReq (..),
    DDriver.OnboardDriverRes (..),
    DDriver.CreatePerson (..),
    DDriver.CreateVehicle (..),
    DDriver.UpdateDriverReq (..),
    DDriver.UpdateDriverRes,
    DDriver.GetNearbySearchRequestsRes (..),
    DDriver.DriverOfferReq (..),
    API,
    handler,
  )
where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.Driver as DDriver
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id, state)
import Servant
import Utils.Auth

type API =
  "org" :> "driver"
    :> ( AdminTokenAuth
           :> ReqBody '[JSON] DDriver.OnboardDriverReq
           :> Post '[JSON] DDriver.OnboardDriverRes
           :<|> "list"
             :> AdminTokenAuth
             :> QueryParam "searchString" Text
             :> QueryParam "limit" Integer
             :> QueryParam "DDriver" Integer
             :> Get '[JSON] DDriver.ListDriverRes
           :<|> AdminTokenAuth
             :> Capture "driverId" (Id SP.Person)
             :> MandatoryQueryParam "enabled" Bool
             :> Post '[JSON] APISuccess
           :<|> AdminTokenAuth
             :> Capture "driverId" (Id SP.Person)
             :> Delete '[JSON] APISuccess
       )
    :<|> "driver"
      :> ( "setActivity"
             :> TokenAuth
             :> MandatoryQueryParam "active" Bool
             :> Post '[JSON] APISuccess
             :<|> "nearbyRideRequest"
               :> ( TokenAuth
                      :> Get '[JSON] DDriver.GetNearbySearchRequestsRes
                  )
             :<|> "searchRequest"
               :> ( TokenAuth
                      :> "quote"
                      :> "offer"
                      :> ReqBody '[JSON] DDriver.DriverOfferReq
                      :> Post '[JSON] APISuccess
                  )
             :<|> "profile"
               :> ( TokenAuth
                      :> Get '[JSON] DDriver.DriverInformationRes
                      :<|> TokenAuth
                        :> ReqBody '[JSON] DDriver.UpdateDriverReq
                        :> Post '[JSON] DDriver.UpdateDriverRes
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
             :<|> getNearbySearchRequests
             :<|> offerQuote
             :<|> ( getInformation
                      :<|> updateDriver
                  )
         )

createDriver :: SP.Person -> DDriver.OnboardDriverReq -> FlowHandler DDriver.OnboardDriverRes
createDriver admin = withFlowHandlerAPI . DDriver.createDriver admin

getInformation :: Id SP.Person -> FlowHandler DDriver.DriverInformationRes
getInformation = withFlowHandlerAPI . DDriver.getInformation

setActivity :: Id SP.Person -> Bool -> FlowHandler APISuccess
setActivity personId = withFlowHandlerAPI . DDriver.setActivity personId

listDriver :: SP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DDriver.ListDriverRes
listDriver admin mbSearchString mbLimit = withFlowHandlerAPI . DDriver.listDriver admin mbSearchString mbLimit

changeDriverEnableState :: SP.Person -> Id SP.Person -> Bool -> FlowHandler APISuccess
changeDriverEnableState admin personId = withFlowHandlerAPI . DDriver.changeDriverEnableState admin personId

deleteDriver :: SP.Person -> Id SP.Person -> FlowHandler APISuccess
deleteDriver admin = withFlowHandlerAPI . DDriver.deleteDriver admin

updateDriver :: Id SP.Person -> DDriver.UpdateDriverReq -> FlowHandler DDriver.UpdateDriverRes
updateDriver personId = withFlowHandlerAPI . DDriver.updateDriver personId

getNearbySearchRequests ::
  Id SP.Person ->
  FlowHandler DDriver.GetNearbySearchRequestsRes
getNearbySearchRequests = withFlowHandlerAPI . DDriver.getNearbySearchRequests

offerQuote ::
  Id SP.Person ->
  DDriver.DriverOfferReq ->
  FlowHandler APISuccess
offerQuote driverId = withFlowHandlerAPI . DDriver.offerQuote driverId
