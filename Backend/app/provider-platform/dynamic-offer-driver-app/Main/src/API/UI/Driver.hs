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
    DDriver.DriverEntityRes (..),
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
    listScheduledBookings,
    acceptScheduledBooking,
  )
where

import qualified Domain.Action.UI.Driver as DDriver
import Domain.Types
import qualified Domain.Types.Booking as DRB
import Domain.Types.Common as DI
import Domain.Types.DriverFee (DriverFeeStatus)
import qualified Domain.Types.DriverHomeLocation as DDHL
import Domain.Types.Invoice (InvoicePaymentMode)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Plan as DPlan
import qualified Domain.Types.SearchTry as DTST
import Environment
import EulerHS.Prelude hiding (id, state)
import qualified IssueManagement.Common.UI.Issue as Common
import Kernel.External.Maps (LatLong)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "driver"
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
                    :> QueryParam "searchTryId" (Id DTST.SearchTry)
                    :> Get '[JSON] DDriver.GetNearbySearchRequestsRes
                )
           :<|> "searchRequest"
             :> TokenAuth
             :> Header "x-package" Text
             :> "quote"
             :> "offer"
             :> ReqBody '[JSON] DDriver.DriverOfferReq
             :> Post '[JSON] APISuccess
           :<|> "searchRequest"
             :> TokenAuth
             :> Header "x-package" Text
             :> "quote"
             :> "respond"
             :> Header "x-bundle-version" Version
             :> Header "x-client-version" Version
             :> Header "x-config-version" Version
             :> Header "x-device" Text
             :> ReqBody '[JSON] DDriver.DriverRespondReq
             :> Post '[JSON] APISuccess
           :<|> "profile"
             :> ( TokenAuth
                    :> Header "x-package" Text
                    :> QueryParam "toss" Int
                    :> QueryParam "tenant" Text
                    :> QueryParam "context" Text
                    :> Get '[JSON] DDriver.DriverInformationRes
                    :<|> TokenAuth
                      :> "info"
                      :> Header "x-package" Text
                      :> QueryParam "toss" Int
                      :> QueryParam "tenant" Text
                      :> QueryParam "context" Text
                      :> ReqBody '[JSON] DDriver.UpdateProfileInfoPoints
                      :> Post '[JSON] DDriver.DriverInformationRes
                    :<|> TokenAuth
                      :> Header "x-bundle-version" Version
                      :> Header "x-client-version" Version
                      :> Header "x-config-version" Version
                      :> Header "x-device" Text
                      :> ReqBody '[JSON] DDriver.UpdateDriverReq
                      :> Post '[JSON] DDriver.UpdateDriverRes
                    :<|> "stats"
                      :> TokenAuth
                      :> MandatoryQueryParam "day" Day
                      :> Get '[JSON] DDriver.DriverStatsRes
                    :<|> "images"
                      :> TokenAuth
                      :> Common.IssueUploadAPI
                    :<|> "verify"
                      :> "vpaStatus"
                      :> TokenAuth
                      :> Post '[JSON] APISuccess
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
           :<|> ( "city"
                    :> ReqBody '[JSON] DDriver.GetCityReq
                    :> Post '[JSON] DDriver.GetCityResp
                )
           :<|> "invoice"
             :> TokenAuth
             :> MandatoryQueryParam "from" Day
             :> QueryParam "to" Day
             :> Get '[JSON] [DDriver.DriverFeeResp]
           :<|> "getDummyRideRequest"
             :> ( TokenAuth
                    :> Get '[JSON] APISuccess
                )
           :<|> "scheduledBooking"
             :> "list"
             :> TokenAuth
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> QueryParam "from" Day
             :> QueryParam "to" Day
             :> QueryParam "tripCategory" TripCategory
             :> QueryParam "currentLocation" LatLong
             :> Get '[JSON] DDriver.ScheduledBookingRes
           :<|> "accept"
             :> "scheduledBooking"
             :> TokenAuth
             :> Header "x-package" Text
             :> MandatoryQueryParam "bookingId" (Id DRB.Booking)
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler =
  setActivity
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
             :<|> getInformationV2
             :<|> updateDriver
             :<|> getStats
             :<|> driverProfileImagesUpload
             :<|> verifyVpaStatus
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
    :<|> getCity
    :<|> getDownloadInvoiceData
    :<|> getDummyRideRequest
    :<|> listScheduledBookings
    :<|> acceptScheduledBooking

getInformation :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> FlowHandler DDriver.DriverInformationRes
getInformation (personId, driverId, merchantOpCityId) mbClientId toss tenant context = withFlowHandlerAPI $ DDriver.getInformation (personId, driverId, merchantOpCityId) mbClientId toss tenant context Nothing

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

updateDriver :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> DDriver.UpdateDriverReq -> FlowHandler DDriver.UpdateDriverRes
updateDriver personId mbBundleVersion mbClientVersion mbConfigVersion mbDevice = withFlowHandlerAPI . DDriver.updateDriver personId mbBundleVersion mbClientVersion mbConfigVersion mbDevice

getNearbySearchRequests ::
  (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe (Id DTST.SearchTry) ->
  FlowHandler DDriver.GetNearbySearchRequestsRes
getNearbySearchRequests (personId, driverId, merchantOpCityId) searchTryId = withFlowHandlerAPI $ DDriver.getNearbySearchRequests (personId, driverId, merchantOpCityId) searchTryId

offerQuote ::
  (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Text ->
  DDriver.DriverOfferReq ->
  FlowHandler APISuccess
offerQuote (personId, driverId, merchantOpCityId) clientId = withFlowHandlerAPI . DDriver.offerQuote (personId, driverId, merchantOpCityId) clientId

respondQuote ::
  (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Text ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  DDriver.DriverRespondReq ->
  FlowHandler APISuccess
respondQuote (personId, driverId, merchantOpCityId) clientId mbBundleVersion mbClientVersion mbConfigVersion mbDevice = withFlowHandlerAPI . DDriver.respondQuote (personId, driverId, merchantOpCityId) clientId mbBundleVersion mbClientVersion mbConfigVersion mbDevice

getStats :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Day -> FlowHandler DDriver.DriverStatsRes
getStats day = withFlowHandlerAPI . DDriver.getStats day

updateMetaData :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DDriver.MetaDataReq -> FlowHandler APISuccess
updateMetaData req = withFlowHandlerAPI . DDriver.updateMetaData req

fetchDriverPhoto :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> FlowHandler Text
fetchDriverPhoto ids = withFlowHandlerAPI . DDriver.fetchDriverPhoto ids

uploadDriverPhoto :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DDriver.DriverPhotoUploadReq -> FlowHandler APISuccess
uploadDriverPhoto req = withFlowHandlerAPI . DDriver.driverPhotoUpload req

driverProfileImagesUpload :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Common.IssueMediaUploadReq -> FlowHandler Common.IssueMediaUploadRes
driverProfileImagesUpload ids = withFlowHandlerAPI . DDriver.driverProfileImagesUpload ids

verifyVpaStatus :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
verifyVpaStatus ids = withFlowHandlerAPI $ DDriver.verifyVpaStatus ids

validate :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DDriver.DriverAlternateNumberReq -> FlowHandler DDriver.DriverAlternateNumberRes
validate alternateNumber = withFlowHandlerAPI . DDriver.validate alternateNumber

verifyAuth :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DDriver.DriverAlternateNumberOtpReq -> FlowHandler APISuccess
verifyAuth otp = withFlowHandlerAPI . DDriver.verifyAuth otp

resendOtp :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> DDriver.DriverAlternateNumberReq -> FlowHandler DDriver.ResendAuth
resendOtp req = withFlowHandlerAPI . DDriver.resendOtp req

remove :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
remove = withFlowHandlerAPI . DDriver.remove

getDriverPayments :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Day -> Maybe Day -> Maybe DriverFeeStatus -> Maybe Int -> Maybe Int -> FlowHandler [DDriver.DriverPaymentHistoryResp]
getDriverPayments authInfo mbFrom mbTo mbStatus mbLimit mbOffset = withFlowHandlerAPI $ DDriver.getDriverPayments authInfo mbFrom mbTo mbStatus mbLimit mbOffset DPlan.YATRI_SUBSCRIPTION

clearDriverDues :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DDriver.ClearDuesRes
clearDriverDues authInfo = withFlowHandlerAPI $ DDriver.clearDriverDues authInfo DPlan.YATRI_SUBSCRIPTION Nothing Nothing

getDriverPaymentsHistoryV2 :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe InvoicePaymentMode -> Maybe Int -> Maybe Int -> FlowHandler DDriver.HistoryEntityV2
getDriverPaymentsHistoryV2 authInfo pMode mbLimit mbOffset = withFlowHandlerAPI $ DDriver.getDriverPaymentsHistoryV2 authInfo pMode mbLimit mbOffset DPlan.YATRI_SUBSCRIPTION

getDriverPaymentsHistoryEntityDetailsV2 :: Text -> (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler DDriver.HistoryEntryDetailsEntityV2
getDriverPaymentsHistoryEntityDetailsV2 invoiceId authInfo = withFlowHandlerAPI $ DDriver.getHistoryEntryDetailsEntityV2 authInfo invoiceId DPlan.YATRI_SUBSCRIPTION

getCity :: DDriver.GetCityReq -> FlowHandler DDriver.GetCityResp
getCity = withFlowHandlerAPI . DDriver.getCity

getDownloadInvoiceData :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Day -> Maybe Day -> FlowHandler [DDriver.DriverFeeResp]
getDownloadInvoiceData (personId, merchantId, merchantOpCityId) fromDate = withFlowHandlerAPI . DDriver.getDownloadInvoiceData (personId, merchantId, merchantOpCityId) fromDate

getDummyRideRequest :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler APISuccess
getDummyRideRequest = withFlowHandlerAPI . DDriver.getDummyRideRequest

listScheduledBookings :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Integer -> Maybe Integer -> Maybe Day -> Maybe Day -> Maybe TripCategory -> Maybe LatLong -> FlowHandler DDriver.ScheduledBookingRes
listScheduledBookings (personId, merchantId, merchantOpCityId) mbLimit mbOffset mbFromDay mbToDay mbTripCategory mbDLoc = withFlowHandlerAPI $ DDriver.listScheduledBookings (personId, merchantId, merchantOpCityId) mbLimit mbOffset mbFromDay mbToDay mbTripCategory mbDLoc

acceptScheduledBooking :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Text -> Id DRB.Booking -> FlowHandler APISuccess
acceptScheduledBooking (personId, merchantId, merchantOpCityId) clientId bookingId = withFlowHandlerAPI $ DDriver.acceptScheduledBooking (personId, merchantId, merchantOpCityId) clientId bookingId

getInformationV2 :: (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> DDriver.UpdateProfileInfoPoints -> FlowHandler DDriver.DriverInformationRes
getInformationV2 (personId, driverId, merchantOpCityId) mbClientId toss tenant context req = withFlowHandlerAPI $ DDriver.getInformationV2 (personId, driverId, merchantOpCityId) mbClientId toss tenant context req
