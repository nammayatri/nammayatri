-- | The shared-cab feature's public surface: modules outside `SharedLogic.SharedCab.*` import this one (exceptions: the API
-- handlers, scheduler jobs, generated code, and modules below it in the import graph).
module SharedLogic.SharedCab
  ( -- * Tier predicates
    isSharedCabBooking,
    isSharedCabAgency,
    filterCabLegs,
    sharedCabFareTiers,
    recordSearch,
    sharedCabBoardStops,
    walkUpVehicleData,
    routeServiceability,
    cabRouteStations,
    SharedCabConfirmFlow,
    onBookingCreated,
    confirmHook,
    getSharedCabLegState,
    noBookingLegState,
    SharedCabBoardOutcome (..),
    tryBoardSharedCab,
    boardingResponse,
    CancelStage (..),
    guardRiderCancel,
    dropRider,
    Refund,
    cancelRefund,
    readRefundDecision,
    refundAmounts,
    refundWithheld,
  )
where

import SharedLogic.SharedCab.Boarding (SharedCabBoardOutcome (..), boardingResponse, tryBoardSharedCab)
import SharedLogic.SharedCab.Cancel (CancelStage (..), dropRider, guardRiderCancel)
import SharedLogic.SharedCab.ConfirmHook (SharedCabConfirmFlow, confirmHook, onBookingCreated)
import SharedLogic.SharedCab.Demand (recordSearch, sharedCabBoardStops)
import SharedLogic.SharedCab.JourneyLeg (getSharedCabLegState, noBookingLegState)
import SharedLogic.SharedCab.LegState (isSharedCabAgency, sharedCabFareTiers)
import SharedLogic.SharedCab.RefundDecision (Refund, cancelRefund, isSharedCabBooking, readRefundDecision, refundAmounts, refundWithheld)
import SharedLogic.SharedCab.Session (filterCabLegs)
import SharedLogic.SharedCab.WalkUp (cabRouteStations, routeServiceability, walkUpVehicleData)
