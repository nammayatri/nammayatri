{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
    DDriver.DriverRespondReq (..),
    DDriver.DriverStatsRes (..),
    DDriver.DriverAlternateNumberReq (..),
    DDriver.DriverAlternateNumberRes (..),
    DDriver.DriverAlternateNumberOtpReq (..),
    DDriver.ResendAuth (..),
    API,
    handler,
  )
where

import Data.Time (Day)
import qualified Domain.Action.UI.Driver as DDriver
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id, state)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

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
               :> TokenAuth
               :> "quote"
               :> "offer"
               :> ReqBody '[JSON] DDriver.DriverOfferReq
               :> Post '[JSON] APISuccess
             :<|> "searchRequest"
               :> TokenAuth
               :> "quote"
               :> "respond"
               :> ReqBody '[JSON] DDriver.DriverRespondReq
               :> Post '[JSON] APISuccess
             :<|> "profile"
               :> ( TokenAuth
                      :> Get '[JSON] DDriver.DriverInformationRes
                      :<|> TokenAuth
                        :> ReqBody '[JSON] DDriver.UpdateDriverReq
                        :> Post '[JSON] DDriver.UpdateDriverRes
                      :<|> "stats"
                        :> TokenAuth
                        :> MandatoryQueryParam "day" Day
                        :> Get '[JSON] DDriver.DriverStatsRes
                  )
             :<|> "alternateNumber"
               :> ( "validate"
                      :> TokenAuth
                      :> ReqBody '[JSON] DDriver.DriverAlternateNumberReq
                      :> Post '[JSON] DDriver.DriverAlternateNumberRes
                      :<|> "verify"
                        :> TokenAuth
                        :> ReqBody '[JSON] DDriver.DriverAlternateNumberOtpReq
                        :> Post '[JSON] APISuccess
                      :<|> "resendOtp"
                        :> TokenAuth
                        :> ReqBody '[JSON] DDriver.DriverAlternateNumberReq
                        :> Post '[JSON] DDriver.ResendAuth
                      :<|> "remove"
                        :> TokenAuth
                        :> Post '[JSON] APISuccess
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
             :<|> respondQuote
             :<|> ( getInformation
                      :<|> updateDriver
                      :<|> getStats
                  )
             :<|> validate
             :<|> verifyAuth
             :<|> resendOtp
             :<|> remove
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

respondQuote ::
  Id SP.Person ->
  DDriver.DriverRespondReq ->
  FlowHandler APISuccess
respondQuote driverId = withFlowHandlerAPI . DDriver.respondQuote driverId

getStats :: Id SP.Person -> Day -> FlowHandler DDriver.DriverStatsRes
getStats day = withFlowHandlerAPI . DDriver.getStats day

validate :: Id SP.Person -> DDriver.DriverAlternateNumberReq -> FlowHandler DDriver.DriverAlternateNumberRes
validate alternateNumber = withFlowHandlerAPI . DDriver.validate alternateNumber

verifyAuth :: Id SP.Person -> DDriver.DriverAlternateNumberOtpReq -> FlowHandler APISuccess
verifyAuth otp = withFlowHandlerAPI . DDriver.verifyAuth otp

resendOtp :: Id SP.Person -> DDriver.DriverAlternateNumberReq -> FlowHandler DDriver.ResendAuth
resendOtp req = withFlowHandlerAPI . DDriver.resendOtp req

remove :: Id SP.Person -> FlowHandler APISuccess
remove = withFlowHandlerAPI . DDriver.remove
