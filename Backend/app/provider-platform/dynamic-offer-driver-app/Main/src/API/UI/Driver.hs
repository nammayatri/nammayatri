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
    DDriver.AddHomeLocationReq (..),
    DDriver.GetHomeLocationsRes (..),
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
    DDriver.DriverPhotoUploadReq (..),
    DDriver.ResendAuth (..),
    DDriver.MetaDataReq (..),
    API,
    handler,
  )
where

import Data.Time (Day)
import qualified Domain.Action.UI.Driver as DDriver
import qualified Domain.Types.Driver.GoHomeFeature.DriverHomeLocation as DDHL
import Domain.Types.DriverFee (DriverFeeStatus)
import Domain.Types.DriverInformation as DI
import Domain.Types.Invoice (InvoicePaymentMode)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id, state)
import Kernel.External.Maps (LatLong)
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
             :<|> "goHome"
               :> ( "activate" :> TokenAuth
                      :> MandatoryQueryParam "homeLocationId" (Id DDHL.DriverHomeLocation)
                      :> MandatoryQueryParam "currentLocation" LatLong
                      :> Post '[JSON] APISuccess
                      :<|> "deactivate"
                      :> TokenAuth
                      :> Post '[JSON] APISuccess
                      :<|> "add"
                      :> TokenAuth
                      :> ReqBody '[JSON] DDriver.AddHomeLocationReq
                      :> Post '[JSON] APISuccess
                      :<|> "get"
                      :> TokenAuth
                      :> Get '[JSON] DDriver.GetHomeLocationsRes
                      :<|> "delete"
                      :> TokenAuth
                      :> MandatoryQueryParam "homeLocationId" (Id DDHL.DriverHomeLocation)
                      :> Delete '[JSON] APISuccess
                      :<|> "update"
                      :> TokenAuth
                      :> MandatoryQueryParam "homeLocationId" (Id DDHL.DriverHomeLocation)
                      :> ReqBody '[JSON] DDriver.UpdateHomeLocationReq
                      :> Post '[JSON] APISuccess
                  )
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
                      :<|> "photo"
                        :> ( TokenAuth
                               :> ReqBody '[JSON] DDriver.DriverPhotoUploadReq
                               :> Post '[JSON] APISuccess
                               :<|> "media"
                                 :> TokenAuth
                                 :> MandatoryQueryParam "filePath" Text
                                 :> Get '[JSON] Text
                           )
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
             :<|> "cleardues"
               :> TokenAuth
               :> Get '[JSON] DDriver.ClearDuesRes
             :<|> "v2"
               :> "payments"
               :> "history"
               :> TokenAuth
               :> QueryParam "paymentMode" InvoicePaymentMode
               :> QueryParam "limit" Int
               :> QueryParam "offset" Int
               :> Get '[JSON] DDriver.HistoryEntityV2
             :<|> "v2"
               :> "payments"
               :> "history"
               :> Capture "invoiceId" Text
               :> "entity"
               :> TokenAuth
               :> Get '[JSON] DDriver.HistoryEntryDetailsEntityV2
         )

handler :: FlowServer API
handler =
  ( createDriver
      :<|> listDriver
      :<|> changeDriverEnableState
      :<|> deleteDriver
  )
    :<|> ( setActivity
             :<|> ( activateGoHomeFeature
                      :<|> deactivateGoHomeFeature
                      :<|> addHomeLocation
                      :<|> getHomeLocations
                      :<|> deleteHomeLocation
                      :<|> updateHomeLocation
                  )
             :<|> getNearbySearchRequests
             :<|> offerQuote
             :<|> respondQuote
             :<|> ( getInformation
                      :<|> updateDriver
                      :<|> getStats
                      :<|> uploadDriverPhoto
                      :<|> fetchDriverPhoto
                  )
             :<|> updateMetaData
             :<|> ( validate
                      :<|> verifyAuth
                      :<|> resendOtp
                      :<|> remove
                  )
             :<|> getDriverPayments
             :<|> clearDriverDues
             :<|> getDriverPaymentsHistoryV2
             :<|> getDriverPaymentsHistoryEntityDetailsV2
         )

createDriver :: SP.Person -> DDriver.OnboardDriverReq -> FlowHandler DDriver.OnboardDriverRes
createDriver admin = withFlowHandlerAPI . DDriver.createDriver admin

getInformation :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DDriver.DriverInformationRes
getInformation = withFlowHandlerAPI . DDriver.getInformation

setActivity :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Bool -> Maybe DI.DriverMode -> FlowHandler APISuccess
setActivity (personId, driverId, merchantOpCityId) isActive = withFlowHandlerAPI . DDriver.setActivity (personId, driverId, merchantOpCityId) isActive

activateGoHomeFeature :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id DDHL.DriverHomeLocation -> LatLong -> FlowHandler APISuccess
activateGoHomeFeature (personId, driverId, merchantOpCityId) homeLocationId = withFlowHandlerAPI . DDriver.activateGoHomeFeature (personId, driverId, merchantOpCityId) homeLocationId

deactivateGoHomeFeature :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
deactivateGoHomeFeature = withFlowHandlerAPI . DDriver.deactivateGoHomeFeature

addHomeLocation :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DDriver.AddHomeLocationReq -> FlowHandler APISuccess
addHomeLocation (personId, driverId, merchantOpCityId) = withFlowHandlerAPI . DDriver.addHomeLocation (personId, driverId, merchantOpCityId)

updateHomeLocation :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id DDHL.DriverHomeLocation -> DDriver.UpdateHomeLocationReq -> FlowHandler APISuccess
updateHomeLocation (personId, driverId, merchantOpCityId) homeLocationId = withFlowHandlerAPI . DDriver.updateHomeLocation (personId, driverId, merchantOpCityId) homeLocationId

getHomeLocations :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DDriver.GetHomeLocationsRes
getHomeLocations = withFlowHandlerAPI . DDriver.getHomeLocations

deleteHomeLocation :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Id DDHL.DriverHomeLocation -> FlowHandler APISuccess
deleteHomeLocation (personId, driverId, merchantOpCityId) = withFlowHandlerAPI . DDriver.deleteHomeLocation (personId, driverId, merchantOpCityId)

listDriver :: SP.Person -> Maybe Text -> Maybe Integer -> Maybe Integer -> FlowHandler DDriver.ListDriverRes
listDriver admin mbSearchString mbLimit = withFlowHandlerAPI . DDriver.listDriver admin mbSearchString mbLimit

changeDriverEnableState :: SP.Person -> Id SP.Person -> Bool -> FlowHandler APISuccess
changeDriverEnableState admin personId = withFlowHandlerAPI . DDriver.changeDriverEnableState admin personId

deleteDriver :: SP.Person -> Id SP.Person -> FlowHandler APISuccess
deleteDriver admin = withFlowHandlerAPI . DDriver.deleteDriver admin

updateDriver :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DDriver.UpdateDriverReq -> FlowHandler DDriver.UpdateDriverRes
updateDriver personId = withFlowHandlerAPI . DDriver.updateDriver personId

getNearbySearchRequests ::
  (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  FlowHandler DDriver.GetNearbySearchRequestsRes
getNearbySearchRequests = withFlowHandlerAPI . DDriver.getNearbySearchRequests

offerQuote ::
  (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  DDriver.DriverOfferReq ->
  FlowHandler APISuccess
offerQuote (personId, driverId, merchantOpCityId) = withFlowHandlerAPI . DDriver.offerQuote (personId, driverId, merchantOpCityId)

respondQuote ::
  (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  DDriver.DriverRespondReq ->
  FlowHandler APISuccess
respondQuote (personId, driverId, merchantOpCityId) = withFlowHandlerAPI . DDriver.respondQuote (personId, driverId, merchantOpCityId)

getStats :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Day -> FlowHandler DDriver.DriverStatsRes
getStats day = withFlowHandlerAPI . DDriver.getStats day

updateMetaData :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DDriver.MetaDataReq -> FlowHandler APISuccess
updateMetaData req = withFlowHandlerAPI . DDriver.updateMetaData req

fetchDriverPhoto :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> FlowHandler Text
fetchDriverPhoto ids = withFlowHandlerAPI . DDriver.fetchDriverPhoto ids

uploadDriverPhoto :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DDriver.DriverPhotoUploadReq -> FlowHandler APISuccess
uploadDriverPhoto req = withFlowHandlerAPI . DDriver.driverPhotoUpload req

validate :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DDriver.DriverAlternateNumberReq -> FlowHandler DDriver.DriverAlternateNumberRes
validate alternateNumber = withFlowHandlerAPI . DDriver.validate alternateNumber

verifyAuth :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DDriver.DriverAlternateNumberOtpReq -> FlowHandler APISuccess
verifyAuth otp = withFlowHandlerAPI . DDriver.verifyAuth otp

resendOtp :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DDriver.DriverAlternateNumberReq -> FlowHandler DDriver.ResendAuth
resendOtp req = withFlowHandlerAPI . DDriver.resendOtp req

remove :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
remove = withFlowHandlerAPI . DDriver.remove

getDriverPayments :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Day -> Maybe Day -> Maybe DriverFeeStatus -> Maybe Int -> Maybe Int -> FlowHandler [DDriver.DriverPaymentHistoryResp]
getDriverPayments mbFrom mbTo mbStatus mbLimit mbOffset = withFlowHandlerAPI . DDriver.getDriverPayments mbFrom mbTo mbStatus mbLimit mbOffset

clearDriverDues :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DDriver.ClearDuesRes
clearDriverDues = withFlowHandlerAPI . DDriver.clearDriverDues

getDriverPaymentsHistoryV2 :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe InvoicePaymentMode -> Maybe Int -> Maybe Int -> FlowHandler DDriver.HistoryEntityV2
getDriverPaymentsHistoryV2 pMode mbLimit mbOffset = withFlowHandlerAPI . DDriver.getDriverPaymentsHistoryV2 pMode mbLimit mbOffset

getDriverPaymentsHistoryEntityDetailsV2 :: Text -> (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DDriver.HistoryEntryDetailsEntityV2
getDriverPaymentsHistoryEntityDetailsV2 invoiceId (driverId, merchantId, merchantOpCityId) = withFlowHandlerAPI $ DDriver.getHistoryEntryDetailsEntityV2 (driverId, merchantId, merchantOpCityId) invoiceId
