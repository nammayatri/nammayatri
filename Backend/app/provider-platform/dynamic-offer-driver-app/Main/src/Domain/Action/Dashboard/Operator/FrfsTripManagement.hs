module Domain.Action.Dashboard.Operator.FrfsTripManagement
  ( postOperatorFrfsTripAction,
  )
where

import qualified API.Types.ProviderPlatform.Operator.FrfsTripManagement as Common
import qualified API.Types.UI.FRFSFleetOperator as UIFRFS
import qualified Domain.Action.UI.FRFSFleetOperator as FRFSFO
import qualified Domain.Types.FleetOperatorTripAction as DFTA
import qualified Domain.Types.Merchant
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

-- | Operator-triggered FRFS trip action from the dashboard. Access is gated entirely by the dashboard
-- auth (ApiAuthV2 + the access matrix restricting this endpoint's access type to operator roles), so
-- there is no in-handler role check -- matching the TransitOperator dashboard. We invoke the shared
-- trip-action core with checks bypassed (the geofence/lead-time gates are for the driver app only).
postOperatorFrfsTripAction ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.OperatorTripActionReq ->
  Environment.Flow Common.OperatorTripActionRes
postOperatorFrfsTripAction merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let providerReq =
        UIFRFS.FleetOperatorTripActionReq
          { action = toProviderAction req.action,
            gimsConductorId = req.gimsConductorId,
            gimsDriverId = req.gimsDriverId,
            vehicleNumber = req.vehicleNumber,
            location = Nothing
          }
  resp <- FRFSFO.frfsFleetOperatorTripAction True merchantOpCity.id providerReq
  pure Common.OperatorTripActionRes {currentTripNumber = resp.currentTripNumber, hasUpcomingTrips = resp.hasUpcomingTrips}
  where
    toProviderAction :: Common.OperatorFleetTripAction -> DFTA.FleetOperatorTripAction
    toProviderAction a = case a of
      Common.TripStart -> DFTA.TripStart
      Common.TripEnd -> DFTA.TripEnd
      Common.TripReset -> DFTA.TripReset
      Common.TripRollback -> DFTA.TripRollback
