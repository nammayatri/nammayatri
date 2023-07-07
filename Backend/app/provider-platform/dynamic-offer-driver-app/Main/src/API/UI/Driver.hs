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
    DDriver.MetaDataReq (..),
    API,
    handler,
  )
where

import Data.Time (Day)
import qualified Domain.Action.UI.Driver as DDriver
import Domain.Types.DriverFee (DriverFeeStatus)
import Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant as Merchant
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
             :> QueryParam "mode" DI.DriverMode
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
             :<|> "metaData"
               :> TokenAuth
               :> ReqBody '[JSON] DDriver.MetaDataReq
               :> Post '[JSON] APISuccess
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
                        :> Delete '[JSON] APISuccess
                  )
             :<|> "payments"
               :> "history"
               :> TokenAuth
               :> QueryParam "from" Day -- rides with window start date >= from
               :> QueryParam "to" Day -- rides with window end date <= to
               :> QueryParam "status" DriverFeeStatus
               :> QueryParam "limit" Int
               :> QueryParam "offset" Int
               :> Get '[JSON] [DDriver.DriverPaymentHistoryResp]
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
             :<|> updateMetaData
             :<|> ( validate
                      :<|> verifyAuth
                      :<|> resendOtp
                      :<|> remove
                  )
             :<|> getDriverPayments
         )

createDriver :: SP.Person -> DDriver.OnboardDriverReq -> FlowHandler DDriver.OnboardDriverRes
createDriver admin = withFlowHandlerAPI . DDriver.createDriver admin

getInformation :: (Id SP.Person, Id Merchant.Merchant) -> FlowHandler DDriver.DriverInformationRes
getInformation = withFlowHandlerAPI . DDriver.getInformation

setActivity :: (Id SP.Person, Id Merchant.Merchant) -> Bool -> Maybe DI.DriverMode -> FlowHandler APISuccess
setActivity (personId, driverId) mode = withFlowHandlerAPI . DDriver.setActivity (personId, driverId) mode

listDriver :: SP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DDriver.ListDriverRes
listDriver admin mbSearchString mbLimit = withFlowHandlerAPI . DDriver.listDriver admin mbSearchString mbLimit

changeDriverEnableState :: SP.Person -> Id SP.Person -> Bool -> FlowHandler APISuccess
changeDriverEnableState admin personId = withFlowHandlerAPI . DDriver.changeDriverEnableState admin personId

deleteDriver :: SP.Person -> Id SP.Person -> FlowHandler APISuccess
deleteDriver admin = withFlowHandlerAPI . DDriver.deleteDriver admin

updateDriver :: (Id SP.Person, Id Merchant.Merchant) -> DDriver.UpdateDriverReq -> FlowHandler DDriver.UpdateDriverRes
updateDriver personId = withFlowHandlerAPI . DDriver.updateDriver personId

getNearbySearchRequests ::
  (Id SP.Person, Id Merchant.Merchant) ->
  FlowHandler DDriver.GetNearbySearchRequestsRes
getNearbySearchRequests = withFlowHandlerAPI . DDriver.getNearbySearchRequests

offerQuote ::
  (Id SP.Person, Id Merchant.Merchant) ->
  DDriver.DriverOfferReq ->
  FlowHandler APISuccess
offerQuote (personId, driverId) = withFlowHandlerAPI . DDriver.offerQuote (personId, driverId)

respondQuote ::
  (Id SP.Person, Id Merchant.Merchant) ->
  DDriver.DriverRespondReq ->
  FlowHandler APISuccess
respondQuote (personId, driverId) = withFlowHandlerAPI . DDriver.respondQuote (personId, driverId)

getStats :: (Id SP.Person, Id Merchant.Merchant) -> Day -> FlowHandler DDriver.DriverStatsRes
getStats day = withFlowHandlerAPI . DDriver.getStats day

updateMetaData :: (Id SP.Person, Id Merchant.Merchant) -> DDriver.MetaDataReq -> FlowHandler APISuccess
updateMetaData req = withFlowHandlerAPI . DDriver.updateMetaData req

validate :: (Id SP.Person, Id Merchant.Merchant) -> DDriver.DriverAlternateNumberReq -> FlowHandler DDriver.DriverAlternateNumberRes
validate alternateNumber = withFlowHandlerAPI . DDriver.validate alternateNumber

verifyAuth :: (Id SP.Person, Id Merchant.Merchant) -> DDriver.DriverAlternateNumberOtpReq -> FlowHandler APISuccess
verifyAuth otp = withFlowHandlerAPI . DDriver.verifyAuth otp

resendOtp :: (Id SP.Person, Id Merchant.Merchant) -> DDriver.DriverAlternateNumberReq -> FlowHandler DDriver.ResendAuth
resendOtp req = withFlowHandlerAPI . DDriver.resendOtp req

remove :: (Id SP.Person, Id Merchant.Merchant) -> FlowHandler APISuccess
remove = withFlowHandlerAPI . DDriver.remove

getDriverPayments :: (Id SP.Person, Id Merchant.Merchant) -> Maybe Day -> Maybe Day -> Maybe DriverFeeStatus -> Maybe Int -> Maybe Int -> FlowHandler [DDriver.DriverPaymentHistoryResp]
getDriverPayments mbFrom mbTo mbStatus mbLimit mbOffset = withFlowHandlerAPI . DDriver.getDriverPayments mbFrom mbTo mbStatus mbLimit mbOffset
