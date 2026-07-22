{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.RideBooking.DriverInfoByPhone
  ( getDriverInfoByPhone,
    orderCitiesForSearch,
    selectSearchCities,
  )
where

import qualified API.Client.ProviderPlatform.RideBooking as Client
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Driver as Common
import Control.Monad.Extra (firstJustM)
import Data.List (sortOn)
import qualified Domain.Action.ProviderPlatform.RideBooking.Driver as RBDriver
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.MerchantAccess as DAccess
import "lib-dashboard" Environment
import EulerHS.Prelude hiding (id, sortOn)
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QMerchantAccess
import Tools.Auth.Api (ApiTokenInfo (..))
import Tools.Auth.Merchant (CheckedShortId, skipMerchantCityAccessCheck)
import qualified "lib-dashboard" Tools.Error as E

-- | Dedup the city list and float the caller's token city to the front, so the
-- common case (driver is in the city the operator is already looking at) costs
-- a single hop. @sortOn@ is stable and @False < True@, so the remaining cities
-- keep their original relative order.
orderCitiesForSearch :: City.City -> [City.City] -> [City.City]
orderCitiesForSearch tokenCity = sortOn (/= tokenCity) . nub

-- | Cities this caller may search: only those granted under the token's own
-- merchant. Filtering here is what makes it safe for the handler to use
-- 'skipMerchantCityAccessCheck' on each per-city call.
selectSearchCities :: Id DM.Merchant -> City.City -> [DAccess.MerchantAccess] -> [City.City]
selectSearchCities tokenMerchantId tokenCity accesses =
  orderCitiesForSearch tokenCity [ma.operatingCity | ma <- accesses, ma.merchantId == tokenMerchantId]

-- | Find a driver by phone across every city the caller has been granted,
-- returning the first hit. The path city is ignored on purpose: the whole point
-- of this endpoint is that the operator does not know which city the driver is in.
getDriverInfoByPhone ::
  ShortId DM.Merchant ->
  City.City ->
  ApiTokenInfo ->
  Maybe Text ->
  Maybe Text ->
  Flow Common.DriverInfoRes
getDriverInfoByPhone merchantShortId _pathCity apiTokenInfo mbMobileNumber mbMobileCountryCode = do
  unless (merchantShortId == apiTokenInfo.merchant.shortId) $ throwError AccessDenied
  mobileNumber <- fromMaybeM (InvalidRequest "\"mobileNumber\" is required") mbMobileNumber
  let mobileCountryCode = fromMaybe "+91" mbMobileCountryCode
  isFleetRequestor <- RBDriver.getRequestorFleetFlag apiTokenInfo
  when isFleetRequestor $ throwError (InvalidRequest "Fleet Owner can only search with vehicle Number, personId or walletId")
  accesses <- QMerchantAccess.findAllMerchantAccessByPersonId apiTokenInfo.personId
  let cities = selectSearchCities apiTokenInfo.merchant.id apiTokenInfo.city accesses
      checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  logInfo $ "getDriverInfoByPhone: searching " <> show (length cities) <> " cities for merchant " <> merchantShortId.getShortId
  firstJustM (tryCity checkedMerchantId apiTokenInfo isFleetRequestor mobileNumber mbMobileCountryCode) cities
    >>= fromMaybeM (PersonDoesNotExist $ mobileCountryCode <> mobileNumber)

-- | One per-city attempt. Only a genuine "driver isn't here" domain error is
-- swallowed into 'Nothing'; anything else propagates, so a single city being
-- down surfaces as an error rather than a false "not found".
tryCity ::
  CheckedShortId DM.Merchant ->
  ApiTokenInfo ->
  Bool ->
  Text ->
  Maybe Text ->
  City.City ->
  Flow (Maybe Common.DriverInfoRes)
tryCity checkedMerchantId apiTokenInfo isFleetRequestor mobileNumber mbMobileCountryCode city =
  ( Just
      <$> Client.callRideBookingAPI
        checkedMerchantId
        city
        (.driverDSL.getDriverInfo)
        apiTokenInfo.personId.getId
        isFleetRequestor
        (Just mobileNumber)
        mbMobileCountryCode
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
  )
    `catch` \(err :: E.Error) ->
      if err.contents.errorCode `elem` ["PERSON_DOES_NOT_EXIST", "MERCHANT_OPERATING_CITY_NOT_FOUND"]
        then do
          logInfo $ "getDriverInfoByPhone: driver not found in city " <> show city
          pure Nothing
        else throwError err
