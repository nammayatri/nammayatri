module Domain.Action.ProviderPlatform.Fleet.Driver
  ( postDriverFleetAddVehicle,
    postDriverFleetAddRCWithoutDriver,
    getDriverFleetGetAllVehicle,
    getDriverFleetGetAllDriver,
    postDriverFleetUnlink,
    postDriverFleetRemoveVehicle,
    postDriverFleetRemoveDriver,
    getDriverFleetTotalEarning,
    getDriverFleetVehicleEarning,
    getDriverFleetDriverEarning,
    getDriverFleetDriverVehicleAssociation,
    getDriverFleetDriverAssociation,
    getDriverFleetVehicleAssociation,
    postDriverFleetVehicleDriverRcStatus,
    postDriverUpdateFleetOwnerInfo,
    getDriverFleetOwnerInfo,
    postDriverFleetSendJoiningOtp,
    postDriverFleetVerifyJoiningOtp,
    postDriverFleetLinkRCWithDriver,
    postDriverFleetAddDrivers,
  )
where

import qualified API.Client.ProviderPlatform.Fleet as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.DriverRegistration as Registration
import "lib-dashboard" Domain.Action.Dashboard.Person as DPerson
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import EulerHS.Prelude
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Person as QP
import Tools.Auth.Api
import Tools.Auth.Merchant

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo ->
  Maybe (Id Common.Driver) ->
  Maybe request ->
  m DT.Transaction
buildTransaction apiTokenInfo driverId =
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) driverId Nothing

postDriverFleetAddVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe Text -> Common.AddVehicleReq -> Flow APISuccess
postDriverFleetAddVehicle merchantShortId opCity apiTokenInfo phoneNo mbMobileCountryCode req = do
  runRequestValidation Common.validateAddVehicleReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetAddVehicle) phoneNo apiTokenInfo.personId.getId mbMobileCountryCode req

postDriverFleetAddRCWithoutDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Registration.RegisterRCReq -> Flow APISuccess
postDriverFleetAddRCWithoutDriver merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetAddRCWithoutDriver) apiTokenInfo.personId.getId req

getDriverFleetGetAllVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Flow Common.ListVehicleRes
getDriverFleetGetAllVehicle merchantShortId opCity apiTokenInfo mbLimit mbOffset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetAllVehicle) apiTokenInfo.personId.getId mbLimit mbOffset

getDriverFleetGetAllDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Flow Common.FleetListDriverRes
getDriverFleetGetAllDriver merchantShortId opCity apiTokenInfo mbLimit mbOffset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetAllDriver) apiTokenInfo.personId.getId mbLimit mbOffset

postDriverFleetUnlink :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Text -> Flow APISuccess
postDriverFleetUnlink merchantShortId opCity apiTokenInfo driverId vehicleNo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetUnlink) apiTokenInfo.personId.getId driverId vehicleNo

postDriverFleetRemoveVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Flow APISuccess
postDriverFleetRemoveVehicle merchantShortId opCity apiTokenInfo vehicleNo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRemoveVehicle) apiTokenInfo.personId.getId vehicleNo

postDriverFleetRemoveDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverFleetRemoveDriver merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRemoveDriver) apiTokenInfo.personId.getId driverId

getDriverFleetTotalEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.FleetTotalEarningResponse
getDriverFleetTotalEarning merchantShortId opCity apiTokenInfo mbFrom mbTo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetTotalEarning) apiTokenInfo.personId.getId mbFrom mbTo

getDriverFleetVehicleEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.FleetEarningListRes
getDriverFleetVehicleEarning merchantShortId opCity apiTokenInfo mbVehicleNo mbLimit mbOffset mbFrom mbTo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetVehicleEarning) apiTokenInfo.personId.getId mbVehicleNo mbLimit mbOffset mbFrom mbTo

getDriverFleetDriverEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Common.SortOn -> Flow Common.FleetEarningListRes
getDriverFleetDriverEarning merchantShortId opCity apiTokenInfo mbMobileCountryCode mbMobileNo mbLimit mbOffset mbFrom mbTo mbSortDesc mbSortOn = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverEarning) apiTokenInfo.personId.getId mbMobileCountryCode mbMobileNo mbLimit mbOffset mbFrom mbTo mbSortDesc mbSortOn

getDriverFleetDriverVehicleAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.DrivertoVehicleAssociationRes
getDriverFleetDriverVehicleAssociation merchantId opCity apiTokenInfo mbLimit mbOffset mbCountryCode mbPhoneNo mbVehicleNo mbStatus mbFrom mbTo = do
  checkedMerchantId <- merchantCityAccessCheck merchantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverVehicleAssociation) apiTokenInfo.personId.getId mbLimit mbOffset mbCountryCode mbPhoneNo mbVehicleNo mbStatus mbFrom mbTo

getDriverFleetDriverAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.DriverMode -> Flow Common.DrivertoVehicleAssociationRes
getDriverFleetDriverAssociation merhcantId opCity apiTokenInfo mbIsActive mbLimit mbOffset mbCountryCode mbPhoneNo mbStats mbFrom mbTo mbMode = do
  checkedMerchantId <- merchantCityAccessCheck merhcantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverAssociation) apiTokenInfo.personId.getId mbIsActive mbLimit mbOffset mbCountryCode mbPhoneNo mbStats mbFrom mbTo mbMode

getDriverFleetVehicleAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.FleetVehicleStatus -> Flow Common.DrivertoVehicleAssociationRes
getDriverFleetVehicleAssociation merhcantId opCity apiTokenInfo mbLimit mbOffset mbVehicleNo mbIncludeStats mbFrom mbTo mbStatus = do
  checkedMerchantId <- merchantCityAccessCheck merhcantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetVehicleAssociation) apiTokenInfo.personId.getId mbLimit mbOffset mbVehicleNo mbIncludeStats mbFrom mbTo mbStatus

postDriverFleetVehicleDriverRcStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.RCStatusReq -> Flow APISuccess
postDriverFleetVehicleDriverRcStatus merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $
    Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetVehicleDriverRcStatus) driverId apiTokenInfo.personId.getId req

postDriverUpdateFleetOwnerInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateFleetOwnerInfoReq -> Flow APISuccess
postDriverUpdateFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $ do
    unless (apiTokenInfo.personId.getId == driverId.getId) $
      throwError AccessDenied
    _ <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverUpdateFleetOwnerInfo) driverId req
    let updateDriverReq =
          DPerson.UpdatePersonReq
            { firstName = req.firstName,
              lastName = req.lastName,
              email = req.email,
              mobileCountryCode = req.mobileCountryCode,
              mobileNumber = req.mobileNo
            }
    _ <- DPerson.updatePerson apiTokenInfo.personId updateDriverReq
    pure Success

getDriverFleetOwnerInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow Common.FleetOwnerInfoRes
getDriverFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId = do
  unless (apiTokenInfo.personId.getId == driverId.getId) $
    throwError AccessDenied
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetOwnerInfo) driverId

postDriverFleetSendJoiningOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Registration.AuthReq -> Flow Registration.AuthRes
postDriverFleetSendJoiningOtp merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  person <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound apiTokenInfo.personId.getId)
  let dashboardUserName = person.firstName <> " " <> person.lastName
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetSendJoiningOtp) dashboardUserName req

postDriverFleetVerifyJoiningOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.VerifyFleetJoiningOtpReq -> Flow APISuccess
postDriverFleetVerifyJoiningOtp merchantShortId opCity apiTokenInfo mbAuthId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetVerifyJoiningOtp) apiTokenInfo.personId.getId mbAuthId req

postDriverFleetLinkRCWithDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.LinkRCWithDriverForFleetReq -> Flow APISuccess
postDriverFleetLinkRCWithDriver merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetLinkRCWithDriver) apiTokenInfo.personId.getId req

postDriverFleetAddDrivers :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.CreateDriversReq -> Flow APISuccess
postDriverFleetAddDrivers merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetAddDrivers) apiTokenInfo.personId.getId req
