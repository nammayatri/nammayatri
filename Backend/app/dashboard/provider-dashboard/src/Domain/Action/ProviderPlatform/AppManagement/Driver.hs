module Domain.Action.ProviderPlatform.AppManagement.Driver (getDriverFleetListRides) where

import qualified API.Client.ProviderPlatform.AppManagement
import qualified Data.Time.Calendar
import qualified "dynamic-offer-driver-app" Domain.Action.UI.Ride
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "dynamic-offer-driver-app" Domain.Types.Person
import qualified "dynamic-offer-driver-app" Domain.Types.Ride
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getDriverFleetListRides :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.Ride.RideStatus -> Kernel.Prelude.Maybe Data.Time.Calendar.Day -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow Domain.Action.UI.Ride.DriverRideListRes)
getDriverFleetListRides merchantShortId opCity apiTokenInfo driverId limit offset onlyActive status day fleetOwnerId numOfDays = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.driverDSL.getDriverFleetListRides) driverId limit offset onlyActive status day fleetOwnerId numOfDays
