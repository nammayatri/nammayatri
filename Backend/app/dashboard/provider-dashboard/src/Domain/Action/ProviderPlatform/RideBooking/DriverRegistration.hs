module Domain.Action.ProviderPlatform.RideBooking.DriverRegistration
  ( postDriverRegistrationAuth,
    postDriverRegistrationVerify,
  )
where

import qualified API.Client.ProviderPlatform.RideBooking as Client
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.DriverRegistration as Common
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Role as DRole
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.CachedQueries.Role as CQRole
import "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Tools.Auth
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error

postDriverRegistrationAuth :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.AuthReq -> Flow Common.AuthRes
postDriverRegistrationAuth merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callRideBookingAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationAuth) req

postDriverRegistrationVerify :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Common.AuthVerifyReq -> Flow APISuccess
postDriverRegistrationVerify merchantShortId opCity apiTokenInfo authId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  encPerson <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound apiTokenInfo.personId.getId)
  role <- CQRole.findById encPerson.roleId >>= fromMaybeM (RoleNotFound encPerson.roleId.getId)
  let mbFleet = role.dashboardAccessType == DRole.FLEET_OWNER || role.dashboardAccessType == DRole.RENTAL_FLEET_OWNER
  Client.callRideBookingAPI checkedMerchantId opCity (.driverRegistrationDSL.postDriverRegistrationVerify) authId mbFleet apiTokenInfo.personId.getId req
