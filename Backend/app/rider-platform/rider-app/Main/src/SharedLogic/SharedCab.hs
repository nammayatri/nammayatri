-- | The shared-cab feature's public surface: modules outside `SharedLogic.SharedCab.*` import this one (exceptions: the API
-- handlers, scheduler jobs, generated code, and modules below it in the import graph).
module SharedLogic.SharedCab
  ( -- * Tier predicates
    isSharedCabBooking,
    isSharedCabAgency,
    filterCabLegs,
    requireSharedCabEnabled,
    sharedCabFareTiers,
    recordSearch,
    sharedCabBoardStops,
    walkUpVehicleData,
    routeServiceability,
    trackStopRoutes,
    withoutCabRows,
    cabRouteServiceTier,
    cabRouteStations,
    SharedCabConfirmFlow,
    onBookingCreated,
    recordPreferredCab,
    stagePickedCab,
    confirmHook,
    getSharedCabLegState,
    liveCabLegCities,
    keepChainsAlive,
    noBookingLegState,
    SharedCabBoardOutcome (..),
    tryBoardSharedCab,
    boardingResponse,
    firstUse,
    CancelStage (..),
    guardRiderCancel,
    dropRider,
    Refund,
    cancelRefund,
    refundAmounts,
    refundWithheld,
  )
where

import SharedLogic.SharedCab.Boarding (SharedCabBoardOutcome (..), boardingResponse, firstUse, tryBoardSharedCab)
import SharedLogic.SharedCab.Cancel (CancelStage (..), dropRider, guardRiderCancel)
import SharedLogic.SharedCab.ConfirmHook (SharedCabConfirmFlow, confirmHook, onBookingCreated, recordPreferredCab, stagePickedCab)
import SharedLogic.SharedCab.Demand (recordSearch, sharedCabBoardStops)
import SharedLogic.SharedCab.Enabled (requireSharedCabEnabled)
import SharedLogic.SharedCab.JourneyLeg (getSharedCabLegState, liveCabLegCities, noBookingLegState)
import SharedLogic.SharedCab.LegState (isSharedCabAgency, sharedCabFareTiers)
import SharedLogic.SharedCab.RefundDecision (Refund, cancelRefund, isSharedCabBooking, refundAmounts, refundWithheld)
import SharedLogic.SharedCab.RouteSnapshot (filterCabLegs)
import SharedLogic.SharedCab.Schedule (keepChainsAlive)
import SharedLogic.SharedCab.WalkUp (cabRouteServiceTier, cabRouteStations, routeServiceability, trackStopRoutes, walkUpVehicleData, withoutCabRows)
