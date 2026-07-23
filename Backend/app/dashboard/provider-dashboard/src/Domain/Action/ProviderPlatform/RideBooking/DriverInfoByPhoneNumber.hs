{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.RideBooking.DriverInfoByPhoneNumber
  ( getDriverInfoByPhoneNumber,
    orderCitiesForSearch,
    selectSearchCities,
  )
where

import qualified API.Client.ProviderPlatform.RideBooking as Client
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.RideBooking.Driver as Common
import Data.List (nub, partition)
import qualified Domain.Action.ProviderPlatform.RideBooking.Driver as RBDriver
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.MerchantAccess as DAccess
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QMerchantAccess
import Tools.Auth.Api
import Tools.Auth.Merchant
import "lib-dashboard" Tools.Error as E

-- | Error codes from @dynamic-offer-driver-app@ that mean "this city does not have the
-- driver", as opposed to "the lookup itself failed". Only these are swallowed by the city
-- loop; anything else must propagate, or an outage in one city would silently masquerade
-- as a driver-not-found for the whole search.
driverMissErrorCodes :: [Text]
driverMissErrorCodes = ["PERSON_DOES_NOT_EXIST", "MERCHANT_OPERATING_CITY_NOT_FOUND"]

-- | Deduplicate, then put the caller's token city first; the remaining cities keep their
-- original relative order. The token city is overwhelmingly the likely match, so trying it
-- first makes the typical request a single hop.
orderCitiesForSearch :: City.City -> [City.City] -> [City.City]
orderCitiesForSearch tokenCity cities =
  let (tokenCityMatches, otherCities) = partition (== tokenCity) (nub cities)
   in tokenCityMatches <> otherCities

-- | The cities this request may search: the caller's own 'MerchantAccess' grants for the
-- token's merchant, and nothing else. Widening this to the merchant's supported cities
-- would let a single-city user read driver PII in cities they were never granted.
selectSearchCities :: Id DM.Merchant -> City.City -> [DAccess.MerchantAccess] -> [City.City]
selectSearchCities tokenMerchantId tokenCity accesses =
  orderCitiesForSearch tokenCity [access.operatingCity | access <- accesses, access.merchantId == tokenMerchantId]

-- | Find a driver by phone number without knowing their city, by searching every city the
-- calling dashboard user has been granted access to. Returns the same 'Common.DriverInfoRes'
-- payload as @\/driver\/info@, so it is a drop-in for callers that only hold a phone number.
getDriverInfoByPhoneNumber ::
  ShortId DM.Merchant ->
  ApiTokenInfo ->
  Maybe Text ->
  Maybe Text ->
  Flow Common.DriverInfoRes
getDriverInfoByPhoneNumber merchantShortId apiTokenInfo mbMobileNumber mbMobileCountryCode = do
  -- The city half of merchantCityAccessCheck cannot apply here (there is no single city),
  -- so the merchant half is enforced by hand before bypassing the check.
  unless (merchantShortId == apiTokenInfo.merchant.shortId) $ throwError AccessDenied
  mobileNumber <- fromMaybeM (InvalidRequest "\"mobileNumber\" is required") mbMobileNumber
  isFleetOwner <- RBDriver.getRequestorFleetFlag apiTokenInfo
  -- The driver-app forbids fleet owners from searching by phone. Failing here preserves
  -- that behaviour and avoids one pointless round trip per accessible city.
  when isFleetOwner $ throwError (InvalidRequest "Fleet Owner can only search with vehicle Number, personId or walletId")
  accesses <- QMerchantAccess.findAllMerchantAccessByPersonId apiTokenInfo.personId
  let cities = selectSearchCities apiTokenInfo.merchant.id apiTokenInfo.city accesses
      checkedMerchantId = skipMerchantCityAccessCheck merchantShortId
  logInfo $ "getDriverInfoByPhoneNumber: searching " <> show (length cities) <> " accessible cities of merchant " <> merchantShortId.getShortId
  searchCities checkedMerchantId mobileNumber cities Nothing
  where
    -- Walks the cities in order and stops at the first hit. On a total miss the first
    -- city's error is re-thrown rather than a fresh one: the driver-app builds that
    -- message with a per-city default country code, so reconstructing it here would be
    -- wrong for any merchant outside India.
    searchCities _ mobileNumber [] mbFirstMiss = case mbFirstMiss of
      Just firstMiss -> throwError firstMiss
      Nothing -> throwError $ PersonDoesNotExist (fromMaybe "" mbMobileCountryCode <> mobileNumber)
    searchCities checkedMerchantId mobileNumber (city : remainingCities) mbFirstMiss = do
      result <- tryCity checkedMerchantId apiTokenInfo mobileNumber mbMobileCountryCode city
      case result of
        Right driverInfo -> do
          logInfo $ "getDriverInfoByPhoneNumber: driver found in city " <> show city
          pure driverInfo
        Left missErr -> do
          logInfo $ "getDriverInfoByPhoneNumber: driver not present in city " <> show city
          searchCities checkedMerchantId mobileNumber remainingCities (Just $ fromMaybe missErr mbFirstMiss)

-- | Look the driver up in a single city. A genuine miss becomes a 'Left' so the loop can
-- move on; every other failure propagates untouched. This is safe by construction: a remote
-- domain error arrives as 'E.Error', while a connection failure is thrown as
-- 'ExternalAPICallError', a separate type this handler never sees.
tryCity ::
  CheckedShortId DM.Merchant ->
  ApiTokenInfo ->
  Text ->
  Maybe Text ->
  City.City ->
  Flow (Either E.Error Common.DriverInfoRes)
tryCity checkedMerchantId apiTokenInfo mobileNumber mbMobileCountryCode city =
  ( Right
      <$> Client.callRideBookingAPI
        checkedMerchantId
        city
        (.driverDSL.getDriverInfo)
        apiTokenInfo.personId.getId
        False -- mbFleet: fleet owners are rejected before the loop starts
        (Just mobileNumber)
        mbMobileCountryCode
        Nothing -- vehicleNumber
        Nothing -- dlNumber
        Nothing -- rcNumber
        Nothing -- email
        Nothing -- personId
        Nothing -- walletId
  )
    `catch` \(err :: E.Error) ->
      if err.contents.errorCode `elem` driverMissErrorCodes
        then pure (Left err)
        else throwError err
