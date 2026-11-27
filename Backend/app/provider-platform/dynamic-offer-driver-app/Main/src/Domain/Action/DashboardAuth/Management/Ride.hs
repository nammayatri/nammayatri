{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handler for waiving a ride's cancellation penalty: for a fleet owner the ride has to belong to a fleet their scope holds.
module Domain.Action.DashboardAuth.Management.Ride
  ( postRideWaiverRideCancellationPenalty,
    getRideList,
    getRideListV2,
    getRideInfo,
  )
where

import qualified API.Types.ProviderPlatform.Management.Ride
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Common
import qualified Domain.Action.Dashboard.Management.Ride
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude
import Kernel.External.Encryption (getDbHash)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Servant
import qualified SharedLogic.FleetScope
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride
import Tools.Auth
import Tools.Auth.DashboardUserAuth

postRideWaiverRideCancellationPenalty :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> API.Types.ProviderPlatform.Management.Ride.WaiverRideCancellationPenaltyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRideWaiverRideCancellationPenalty a5 a4 a3 a2 a1 = do
  Kernel.Prelude.when (SharedLogic.FleetScope.isEnforced a3) $ do
    ride <- Storage.Queries.Ride.findById (Kernel.Types.Id.cast a2) >>= fromMaybeM (Kernel.Types.Error.RideDoesNotExist (Kernel.Types.Id.getId a2))
    maybe (Kernel.Utils.Common.throwError Kernel.Types.Error.AccessDenied) (SharedLogic.FleetScope.assertFleetInScope a3 . Kernel.Types.Id.getId) ride.fleetOwnerId
  Domain.Action.Dashboard.Management.Ride.postRideWaiverRideCancellationPenalty a5 a4 a2 a1

-- | What a ride list is asked to filter on that decides whose rides it shows.
data RideFilters = RideFilters
  { fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver),
    driverPhone :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverCountryCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }

data RideListScope = RideListScope
  { filters :: RideFilters,
    requestorId :: Kernel.Prelude.Text,
    -- | The list is held to one driver linked to the requesting operator, and so needs no fleet.
    ownDriverOnly :: Kernel.Prelude.Bool
  }

-- | The filters and requestor a ride list runs with. In ENFORCE:
--
--   * a fleet owner's list is for one fleet, the one named or the only one they have, taken from their scope. It is
--     passed on as the requestor too, so the shared function's own-fleet rule holds.
--   * an operator names a fleet linked to them, or one of their drivers. A driver of a linked fleet is listed
--     within that fleet; a driver linked to them directly, within that driver.
--
-- Otherwise the request is passed on as it came, with the dashboard person as the requestor.
rideListScope ::
  DashboardUser ->
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  RideFilters ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) ->
  Environment.Flow RideListScope
rideListScope dashboardUser merchantShortId city asked mbCustomerPhone mbRideShortId
  | SharedLogic.FleetScope.isEnforced dashboardUser = do
    (fleetId, actingId) <- SharedLogic.FleetScope.scopedFleetOwnerAndRequestorId dashboardUser asked.fleetOwnerId
    limited (RideListScope (asked {fleetOwnerId = Kernel.Prelude.Just fleetId}) actingId Kernel.Prelude.False)
  | SharedLogic.FleetScope.isEnforcedOperator dashboardUser = operatorScope Kernel.Prelude.>>= limited
  | Kernel.Prelude.otherwise = Kernel.Prelude.pure (asOperator asked Kernel.Prelude.False)
  where
    asOperator scopeFilters = RideListScope scopeFilters (Tools.Auth.DashboardUserAuth.dashboardRequestorId dashboardUser)

    limited scope = do
      assertShortRideWithin mbRideShortId scope.filters
      Kernel.Prelude.pure scope

    operatorScope = case asked.fleetOwnerId of
      Kernel.Prelude.Just fleetId -> do
        SharedLogic.FleetScope.assertOperatorOwnsFleet dashboardUser fleetId
        Kernel.Prelude.pure (asOperator asked Kernel.Prelude.False)
      Kernel.Prelude.Nothing -> do
        driver <- namedDriver Kernel.Prelude.>>= fromMaybeM (Kernel.Types.Error.InvalidRequest "Choose a fleet, or name one of your drivers by ID or phone number")
        -- The driver is filtered on by id alone: some of the lookups a phone number goes through ignore the other filters.
        let byDriver mbFleetId = RideFilters mbFleetId (Kernel.Prelude.Just (Kernel.Types.Id.cast driver)) Kernel.Prelude.Nothing Kernel.Prelude.Nothing
        SharedLogic.FleetScope.operatorDriverReach dashboardUser driver Kernel.Prelude.>>= \case
          Kernel.Prelude.Nothing -> Kernel.Utils.Common.throwError Kernel.Types.Error.AccessDenied
          Kernel.Prelude.Just (SharedLogic.FleetScope.ThroughFleet fleetId) -> Kernel.Prelude.pure (asOperator (byDriver (Kernel.Prelude.Just fleetId)) Kernel.Prelude.False)
          Kernel.Prelude.Just SharedLogic.FleetScope.LinkedDirectly -> do
            -- With no fleet to hold it to, a customer search would reach that customer's rides with other drivers.
            Kernel.Prelude.when (Kernel.Prelude.isJust mbCustomerPhone) $
              Kernel.Utils.Common.throwError (Kernel.Types.Error.InvalidRequest "Searching by customer phone number needs a fleet")
            Kernel.Prelude.pure (asOperator (byDriver Kernel.Prelude.Nothing) Kernel.Prelude.True)

    namedDriver = case asked.driverId of
      Kernel.Prelude.Just namedId -> Kernel.Prelude.pure (Kernel.Prelude.Just (Kernel.Types.Id.cast namedId))
      Kernel.Prelude.Nothing -> case asked.driverPhone of
        Kernel.Prelude.Nothing -> Kernel.Prelude.pure Kernel.Prelude.Nothing
        Kernel.Prelude.Just phone -> do
          merchant <- findMerchantByShortId merchantShortId
          countryCode <- case Domain.Action.Dashboard.Common.appendPlusInMobileCountryCode asked.driverCountryCode of
            Kernel.Prelude.Just code -> Kernel.Prelude.pure code
            Kernel.Prelude.Nothing -> do
              operatingCity <-
                CQMOC.findByMerchantIdAndCity merchant.id city
                  Kernel.Prelude.>>= fromMaybeM (Kernel.Types.Error.InvalidRequest "The merchant does not operate in this city")
              Kernel.Prelude.pure (P.getCountryMobileCode operatingCity.country)
          phoneHash <- getDbHash phone
          Kernel.Prelude.fmap (.id) Kernel.Prelude.<$> QPerson.findByMobileNumberAndMerchantAndRole countryCode phoneHash merchant.id DP.DRIVER

-- | The ride list's lookup by short id does not apply all the filters (the V2 one applies none), so for a limited
-- caller the ride it would find is checked against them here.
assertShortRideWithin :: Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> RideFilters -> Environment.Flow ()
assertShortRideWithin mbRideShortId scopeFilters =
  Kernel.Prelude.whenJust mbRideShortId $ \shortId -> do
    mbRide <- Storage.Queries.Ride.findRideByRideShortId (Kernel.Types.Id.ShortId shortId.getShortId)
    Kernel.Prelude.whenJust mbRide $ \ride -> do
      let ofFleet = Kernel.Prelude.maybe Kernel.Prelude.True (\fleetId -> Kernel.Prelude.fmap Kernel.Types.Id.getId ride.fleetOwnerId Kernel.Prelude.== Kernel.Prelude.Just fleetId) scopeFilters.fleetOwnerId
          ofDriver = Kernel.Prelude.maybe Kernel.Prelude.True (\namedId -> Kernel.Types.Id.getId namedId Kernel.Prelude.== Kernel.Types.Id.getId ride.driverId) scopeFilters.driverId
      Kernel.Prelude.unless (ofFleet Kernel.Prelude.&& ofDriver) $
        Kernel.Utils.Common.throwError (Kernel.Types.Error.InvalidRequest "Ride not found under fleet")

-- Either way a ride list covers the cities the request names, each of which the caller must hold. Naming none, a
-- fleet-bound caller gets every city they hold and anyone else the city in the URL.
getRideList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.BookingStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.PaymentMode -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Environment.Flow API.Types.ProviderPlatform.Management.Ride.RideListRes)
getRideList a21 a20 a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  cities <- SharedLogic.FleetScope.reachableCities a19 a21.getShortId a20 a17
  scope <- rideListScope a19 a21 a20 (RideFilters a10 a12 a11 a13) a14 a3
  Domain.Action.Dashboard.Management.Ride.getRideListInCities (SharedLogic.FleetScope.isEnforced a19) cities scope.ownDriverOnly a21 a20 scope.requestorId a18 a16 a15 a14 scope.filters.driverCountryCode scope.filters.driverId scope.filters.driverPhone scope.filters.fleetOwnerId a9 a8 a7 a6 a5 a4 a3 a2 a1

getRideListV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.PaymentMode -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Ride) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.Ride) -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Ride.RideStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Environment.Flow API.Types.ProviderPlatform.Management.Ride.RideListResV2)
getRideListV2 a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  cities <- SharedLogic.FleetScope.reachableCities a17 a19.getShortId a18 a16
  scope <- rideListScope a17 a19 a18 (RideFilters a11 a13 a12 Kernel.Prelude.Nothing) a14 a4
  Domain.Action.Dashboard.Management.Ride.getRideListV2InCities (SharedLogic.FleetScope.isEnforced a17) cities scope.ownDriverOnly a19 a18 scope.requestorId a15 a14 scope.filters.driverId scope.filters.driverPhone scope.filters.fleetOwnerId a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

-- A fleet's rides can sit in any of its cities, so once fleet scope is enforced the ride is opened in its own city,
-- provided it belongs to a fleet in the caller's scope; the city in the URL does not decide.
getRideInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Ride -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow API.Types.ProviderPlatform.Management.Ride.RideInfoRes)
getRideInfo a5 a4 a3 a2 a1
  | SharedLogic.FleetScope.isEnforced a3 = do
    ride <- Storage.Queries.Ride.findById (Kernel.Types.Id.cast a2) >>= fromMaybeM (Kernel.Types.Error.RideDoesNotExist (Kernel.Types.Id.getId a2))
    maybe (Kernel.Utils.Common.throwError Kernel.Types.Error.AccessDenied) (SharedLogic.FleetScope.assertFleetInScope a3 . Kernel.Types.Id.getId) ride.fleetOwnerId
    Domain.Action.Dashboard.Management.Ride.getRideInfoInRideCity a5 a2 a1
  | SharedLogic.FleetScope.isEnforcedOperator a3 = do
    ride <- Storage.Queries.Ride.findById (Kernel.Types.Id.cast a2) >>= fromMaybeM (Kernel.Types.Error.RideDoesNotExist (Kernel.Types.Id.getId a2))
    SharedLogic.FleetScope.assertOperatorMayReadRide a3 ride
    Domain.Action.Dashboard.Management.Ride.getRideInfo a5 a4 a2 a1
  | Kernel.Prelude.otherwise = Domain.Action.Dashboard.Management.Ride.getRideInfo a5 a4 a2 a1
