{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for direct-dashboard routes whose request needs more
-- than the verified operator's id or name: fleet-owner resolution, fleet-owner
-- verification, dashboard-database writes after the call, and similar.
--
-- provider-dashboard did this work in its own hand-written
-- @Domain.Action.ProviderPlatform.*@ layer before forwarding the call. The
-- generated @API.Action.DashboardAuth@ handler calls these functions instead of
-- the domain handler for every endpoint marked @appServerHandler: custom@ in
-- the spec, so this logic lives here and is never overwritten by the generator.
module Domain.Action.DashboardAuth.RideBooking.Driver
  ( getDriverInfo,
    postDriverCollectCash,
    postDriverExemptCash,
  )
where

import qualified API.Types.Dashboard.RideBooking.Driver
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import qualified Data.List
import qualified Domain.Action.Dashboard.RideBooking.Driver
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.FleetScope
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity
import qualified Storage.Queries.Person
import Tools.Auth
import Tools.Auth.DashboardUserAuth
import qualified "lib-dashboard" Tools.Auth.FleetScope as LibFleetScope
import Tools.Error

getDriverInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.Dashboard.RideBooking.Driver.DriverInfoRes)
-- The driver is looked for in each city in turn, the URL's first when it is one of them. The cities are the ones
-- the request names, each of which the caller must be able to reach. Naming none, it is the city in the URL.
getDriverInfo a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1
  -- The shared function finds the driver itself and, for a fleet caller, guards the lookup by asking the caller to
  -- BE the fleet that holds them. Once fleet scope is enforced that guard is left off and the driver it found is
  -- checked against the fleets in the caller's scope before anything is returned.
  | SharedLogic.FleetScope.isEnforced a10 = do
    -- Naming no city, a driver named by id is looked up in their own city, once they are known to be in scope, and
    -- one named any other way in the cities the caller holds. So a fleet's drivers can be opened from any of its
    -- cities.
    cities <- SharedLogic.FleetScope.reachableCities a10 a12.getShortId a11 a1
    let lookupIn opCity =
          Domain.Action.Dashboard.RideBooking.Driver.getDriverInfo a12 opCity (Tools.Auth.DashboardUserAuth.dashboardRequestorId a10) Kernel.Prelude.False a9 a8 a7 a6 a5 a4 a3 a2
    res <- case a3 of
      Kernel.Prelude.Just driverId
        | Kernel.Prelude.null (LibFleetScope.requestedCities a1) -> do
          let personId = Kernel.Types.Id.cast driverId
          SharedLogic.FleetScope.assertDriverInScope a10 personId
          person <- Storage.Queries.Person.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
          opCityRec <-
            Storage.CachedQueries.Merchant.MerchantOperatingCity.findById person.merchantOperatingCityId
              >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
          lookupIn opCityRec.city
      _ -> findInCities lookupIn a11 (citiesToSearch a11 cities)
    SharedLogic.FleetScope.assertDriverInScope a10 (Kernel.Types.Id.cast res.driverId)
    Kernel.Prelude.pure res
  | Kernel.Prelude.otherwise = do
    cities <- SharedLogic.FleetScope.reachableCities a10 a12.getShortId a11 a1
    mbFleet <- Tools.Auth.DashboardUserAuth.requestorFleetFlag a10
    let lookupIn opCity =
          Domain.Action.Dashboard.RideBooking.Driver.getDriverInfo a12 opCity (Tools.Auth.DashboardUserAuth.dashboardRequestorId a10) mbFleet a9 a8 a7 a6 a5 a4 a3 a2
    findInCities lookupIn a11 (citiesToSearch a11 cities)

-- | The cities to look in, in order: the URL's city when the list holds it or is empty, then the rest.
citiesToSearch :: Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> [Kernel.Types.Beckn.Context.City]
citiesToSearch urlCity mbCities =
  case Data.List.nub (LibFleetScope.requestedCities mbCities) of
    [] -> [urlCity]
    cities ->
      let (urlFirst, others) = Data.List.partition (Kernel.Prelude.== urlCity) cities
       in urlFirst Kernel.Prelude.<> others

-- | The first of these cities that has the driver. If none does, the first city's error is the answer. With no
-- cities to search, the fallback city is asked.
findInCities :: (Kernel.Types.Beckn.Context.City -> Environment.Flow a) -> Kernel.Types.Beckn.Context.City -> [Kernel.Types.Beckn.Context.City] -> Environment.Flow a
findInCities lookupIn fallbackCity = \case
  [] -> lookupIn fallbackCity
  firstCity : otherCities ->
    attempt firstCity Kernel.Prelude.>>= \case
      Kernel.Prelude.Right found -> Kernel.Prelude.pure found
      Kernel.Prelude.Left firstMiss -> search firstMiss otherCities
  where
    attempt city =
      Kernel.Prelude.try (lookupIn city) Kernel.Prelude.>>= \case
        Kernel.Prelude.Right found -> Kernel.Prelude.pure (Kernel.Prelude.Right found)
        Kernel.Prelude.Left (err :: Kernel.Prelude.SomeException) ->
          if isLookupMiss err
            then Kernel.Prelude.pure (Kernel.Prelude.Left err)
            else Kernel.Prelude.throwM err
    search firstMiss [] = Kernel.Prelude.throwM firstMiss
    search firstMiss (city : rest) =
      attempt city Kernel.Prelude.>>= Kernel.Prelude.either (Kernel.Prelude.const (search firstMiss rest)) Kernel.Prelude.pure

-- | Whether an error from the lookup says only that this city does not have the driver.
isLookupMiss :: Kernel.Prelude.SomeException -> Kernel.Prelude.Bool
isLookupMiss err = case Kernel.Prelude.fromException err of
  Kernel.Prelude.Just (PersonDoesNotExist _) -> Kernel.Prelude.True
  _ -> case Kernel.Prelude.fromException err of
    Kernel.Prelude.Just (VehicleDoesNotExist _) -> Kernel.Prelude.True
    _ -> case Kernel.Prelude.fromException err of
      Kernel.Prelude.Just (MerchantOperatingCityNotFound _) -> Kernel.Prelude.True
      _ -> case Kernel.Prelude.fromException err of
        Kernel.Prelude.Just (InvalidRequest message) ->
          message `Kernel.Prelude.elem` ["License does not exist.", "Registration certificate does not exist.", "Email does not exist."]
        _ -> Kernel.Prelude.False

-- The acting operator is recorded as the collector: provider-dashboard sent its
-- own session's person id in the request body.
postDriverCollectCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverCollectCash a5 a4 a3 a2 a1 = do
  SharedLogic.FleetScope.assertDriverInScope a3 (Kernel.Types.Id.cast a2)
  Domain.Action.Dashboard.RideBooking.Driver.postDriverCollectCash a5 a4 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)

postDriverExemptCash :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverExemptCash a4 a3 a2 a1 = do
  SharedLogic.FleetScope.assertDriverInScope a2 (Kernel.Types.Id.cast a1)
  Domain.Action.Dashboard.RideBooking.Driver.postDriverExemptCash a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)
