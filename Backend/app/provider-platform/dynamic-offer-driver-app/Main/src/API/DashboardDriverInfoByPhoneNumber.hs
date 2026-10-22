{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Driver lookup by phone number, across every city the caller may search.
--
-- Mounted with a merchant but no city -- unlike every other dashboard route --
-- because the whole point is to search across cities. That is also why a blanket
-- @\/{merchantId}\/{city}\/@ ingress rule cannot cover it: it would read "driver"
-- as the city.
--
-- provider-dashboard made one HTTP call per city; here each is a direct call.
-- The dashboard reads (person, role, merchant access) run in the dashboard
-- database, the driver lookups in this server's own.
module API.DashboardDriverInfoByPhoneNumber
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.RideBooking.Driver as Common
import Data.List (nub)
import qualified Domain.Action.Dashboard.RideBooking.Driver as RBDriver
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.MerchantAccess as DAccess
import qualified "lib-dashboard" Domain.Types.Role as DRole
import Environment
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified "lib-dashboard" Storage.Beam.SchemaInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QMerchantAccess
import qualified "lib-dashboard" Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.Role as QRole
import "lib-dashboard" Tools.Auth.Dashboard
import qualified "lib-dashboard" Tools.Auth.Verify as Verify
import "lib-dashboard" Tools.Error as E

type API =
  Capture "merchantId" (ShortId DM.Merchant)
    :> "driver"
    :> "infoByPhoneNumber"
    :> DashboardAuth 'DASHBOARD_USER
    :> QueryParam "mobileNumber" Text
    :> QueryParam "mobileCountryCode" Text
    :> Get '[JSON] Common.DriverInfoRes

handler :: FlowServer API
handler = getDriverInfoByPhoneNumber

-- | Error codes from this server that mean "this city does not have the driver",
-- as opposed to "the lookup itself failed". Only these are swallowed by the city
-- loop; anything else must propagate, or an outage in one city would silently
-- masquerade as a driver-not-found for the whole search.
driverMissErrorCodes :: [Text]
driverMissErrorCodes = ["PERSON_DOES_NOT_EXIST", "MERCHANT_OPERATING_CITY_NOT_FOUND"]

-- | The cities this request may search: the caller's own MerchantAccess grants
-- for the URL merchant, and nothing else. Widening this to the merchant's
-- supported cities would let a single-city user read driver PII in cities they
-- were never granted. One token per person: there is no session city to try
-- first, so the accessible cities are searched in grant order (deduplicated).
selectSearchCities :: Id DM.Merchant -> [DAccess.MerchantAccess] -> [Context.City]
selectSearchCities merchantId accesses =
  nub [access.operatingCity | access <- accesses, access.merchantId == merchantId]

getDriverInfoByPhoneNumber ::
  ShortId DM.Merchant ->
  TokenInfo ->
  Maybe Text ->
  Maybe Text ->
  FlowHandler Common.DriverInfoRes
getDriverInfoByPhoneNumber merchantShortId tokenInfo mbMobileNumber mbMobileCountryCode = withFlowHandlerAPI' $ do
  -- Same authorization the proxy applied, keyed on the capability id.
  void $ runInDashboardDb $ Verify.verifyAccessLevel "PROVIDER_RIDE_BOOKING/DRIVER/GET_DRIVER_INFO" tokenInfo.personId
  (merchant, isFleetOwner, accesses) <- runInDashboardDb $ do
    person <- QP.findById tokenInfo.personId >>= fromMaybeM (E.PersonNotFound tokenInfo.personId.getId)
    role <- QRole.findById person.roleId >>= fromMaybeM (E.RoleNotFound person.roleId.getId)
    -- One token per person: the merchant comes from the URL capture, not the token.
    merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (E.MerchantDoesNotExist merchantShortId.getShortId)
    accesses <- QMerchantAccess.findAllMerchantAccessByPersonId tokenInfo.personId
    pure
      ( merchant,
        role.dashboardAccessType == DRole.FLEET_OWNER || role.dashboardAccessType == DRole.RENTAL_FLEET_OWNER,
        accesses
      )
  -- The caller must hold access to the URL merchant. The city half of a
  -- merchant/city check cannot apply here (there is no single city), so the
  -- merchant half is enforced by hand against merchant_access.
  unless (any (\a -> a.merchantId == merchant.id) accesses) $ throwError E.AccessDenied
  mobileNumber <- fromMaybeM (InvalidRequest "\"mobileNumber\" is required") mbMobileNumber
  -- This server forbids fleet owners from searching by phone. Failing here
  -- preserves that behaviour and avoids one pointless lookup per city.
  when isFleetOwner $ throwError (InvalidRequest "Fleet Owner can only search with vehicle Number, personId or walletId")
  let cities = selectSearchCities merchant.id accesses
  logInfo $ "getDriverInfoByPhoneNumber: searching " <> show (length cities) <> " accessible cities of merchant " <> merchantShortId.getShortId
  searchCities mobileNumber cities Nothing
  where
    -- Walks the cities in order and stops at the first hit. On a total miss the
    -- first city's error is re-thrown rather than a fresh one: the message is
    -- built with a per-city default country code, so reconstructing it here would
    -- be wrong for any merchant outside India.
    searchCities mobileNumber [] mbFirstMiss = case mbFirstMiss of
      Just firstMiss -> throwError firstMiss
      Nothing -> throwError $ E.PersonDoesNotExist (fromMaybe "" mbMobileCountryCode <> mobileNumber)
    searchCities mobileNumber (city : remainingCities) mbFirstMiss = do
      result <- tryCity mobileNumber city
      case result of
        Right driverInfo -> do
          logInfo $ "getDriverInfoByPhoneNumber: driver found in city " <> show city
          pure driverInfo
        Left missErr -> do
          logInfo $ "getDriverInfoByPhoneNumber: driver not present in city " <> show city
          searchCities mobileNumber remainingCities (Just $ fromMaybe missErr mbFirstMiss)

    -- A genuine miss becomes a Left so the loop can move on; every other failure
    -- propagates untouched.
    tryCity mobileNumber city =
      ( Right
          <$> RBDriver.getDriverInfo
            -- Same short id, different phantom: the path capture is typed by the
            -- DASHBOARD's Merchant (it is checked against the token's merchant
            -- above), while this handler wants this server's own. The proxy did
            -- the same conversion implicitly, by serialising it onto the wire.
            (ShortId merchantShortId.getShortId)
            city
            tokenInfo.personId.getId
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
