-- | The shared-cab feature's public surface: modules outside `SharedLogic.SharedCab.*` import this one (exceptions: the API
-- handlers, scheduler jobs, generated code, and modules below it in the import graph).
module SharedLogic.SharedCab
  ( -- * Tier predicates
    isSharedCabBooking,
    isSharedCabAgency,
    legAgencyGtfsId,
    searchAgencyKey,
    filterCabLegs,
    requireSharedCabEnabled,
    sharedCabFareTiers,
    recordSearch,
    walkUpCab,
    declaresSharedCab,
    isSharedCabTier,
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
    CancelStage (..),
    guardRiderCancel,
    dropRider,
    Refund,
    cancelRefund,
    refundAmounts,
    refundWithheld,
  )
where

import SharedLogic.SharedCab.Boarding (SharedCabBoardOutcome (..), boardingResponse, tryBoardSharedCab)
import SharedLogic.SharedCab.Cancel (CancelStage (..), dropRider, guardRiderCancel)
import SharedLogic.SharedCab.ConfirmHook (SharedCabConfirmFlow, confirmHook, onBookingCreated, recordPreferredCab, stagePickedCab)
import SharedLogic.SharedCab.Demand (recordSearch)
import SharedLogic.SharedCab.Enabled (requireSharedCabEnabled)
import SharedLogic.SharedCab.JourneyLeg (getSharedCabLegState, liveCabLegCities, noBookingLegState)
import SharedLogic.SharedCab.LegState (isSharedCabAgency, legAgencyGtfsId, searchAgencyKey, sharedCabFareTiers)
import SharedLogic.SharedCab.RefundDecision (Refund, cancelRefund, isSharedCabBooking, refundAmounts, refundWithheld)
import SharedLogic.SharedCab.RouteSnapshot (filterCabLegs)
import SharedLogic.SharedCab.Schedule (keepChainsAlive)
import SharedLogic.SharedCab.WalkUp (cabRouteServiceTier, cabRouteStations, declaresSharedCab, isSharedCabTier, routeServiceability, trackStopRoutes, walkUpCab, withoutCabRows)
