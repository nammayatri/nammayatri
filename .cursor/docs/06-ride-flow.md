# Complete Ride Flow — 8 Phases

This document describes the end-to-end ride lifecycle, from search to completion and rating.

## Phase 1: Search

**Rider opens app and searches for a ride.**

| Step | Service | Key File |
|------|---------|----------|
| Rider submits search | rider-app | `Domain/Action/UI/Search.hs` |
| Search request created | rider-app | `SharedLogic/Search.hs` |
| Build BECKN search message | rider-app | `Beckn/ACL/Search.hs` |
| Transform search for OnDemand | rider-app | `Beckn/OnDemand/Transformer/Search.hs` |
| Search request sent to BPP | rider-app | `SharedLogic/CallBPP.hs` |
| BPP receives search | driver-app | `Beckn/ACL/Search.hs` |
| Search request validated | driver-app | `Domain/Action/Beckn/Search.hs` |

**SearchRequest status**: `ACTIVE` → stored in DB with pickup/drop locations, distance, duration.

## Phase 2: Driver Discovery & Offers

**BPP finds nearby drivers and sends offers back to BAP.**

| Step | Service | Key File |
|------|---------|----------|
| Allocator picks up search job | driver-app | `SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers.hs` |
| Driver pool computed | driver-app | `SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/DriverPool.hs` |
| Unified pool algorithm | driver-app | `SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/DriverPoolUnified.hs` |
| Search request sent to drivers | driver-app | `SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle.hs` |
| Fare calculated | driver-app | `SharedLogic/FareCalculator.hs` |
| Dynamic pricing applied | driver-app | `SharedLogic/DynamicPricing.hs` |
| Build on_search response | driver-app | `Beckn/ACL/OnSearch.hs` |
| BAP receives on_search | rider-app | `Beckn/ACL/OnSearch.hs` |
| Estimates/quotes stored | rider-app | `Domain/Action/Beckn/OnSearch.hs` |
| Estimates displayed to rider | rider-app | `Domain/Action/UI/Estimate.hs` |

**Two modes**:
- **Auto-assign**: System automatically assigns the first accepting driver
- **Manual selection**: Rider sees driver offers and picks one

## Phase 3: Select & Quote

**Rider selects an estimate or a specific driver offer.**

| Step | Service | Key File |
|------|---------|----------|
| Rider selects estimate | rider-app | `Domain/Action/UI/Select.hs` |
| Build BECKN select message | rider-app | `Beckn/ACL/Select.hs` |
| BPP receives select | driver-app | `Beckn/ACL/Select.hs` |
| BPP processes selection | driver-app | `Domain/Action/Beckn/Select.hs` |
| Build on_select response | driver-app | `Beckn/ACL/OnSelect.hs` |
| BAP receives on_select | rider-app | `Beckn/ACL/OnSelect.hs` |
| Quotes displayed | rider-app | `Domain/Action/UI/Quote.hs` |

## Phase 4: Init & Booking Creation

**Rider initiates booking. Payment details exchanged.**

| Step | Service | Key File |
|------|---------|----------|
| Rider initiates booking | rider-app | `Domain/Action/UI/Confirm.hs` (calls init) |
| Build BECKN init message | rider-app | `Beckn/ACL/Init.hs` |
| Transform init for OnDemand | rider-app | `Beckn/OnDemand/Transformer/Init.hs` |
| BPP receives init | driver-app | `Beckn/ACL/Init.hs` |
| BPP creates booking | driver-app | `Domain/Action/Beckn/Init.hs` |
| Build on_init response | driver-app | `Beckn/ACL/OnInit.hs` |
| BAP receives on_init | rider-app | `Beckn/ACL/OnInit.hs` |
| BAP booking created | rider-app | `Domain/Action/Beckn/OnInit.hs` |

**BookingStatus**: `NEW` (BAP) / `NEW` (BPP)

## Phase 5: Confirm & Driver Assignment

**Rider confirms booking. Driver is assigned.**

| Step | Service | Key File |
|------|---------|----------|
| Rider confirms | rider-app | `Domain/Action/UI/Confirm.hs` |
| Confirm logic | rider-app | `SharedLogic/Confirm.hs` |
| Build BECKN confirm message | rider-app | `Beckn/ACL/Confirm.hs` |
| BPP receives confirm | driver-app | `Beckn/ACL/Confirm.hs` |
| BPP processes confirm | driver-app | `Domain/Action/Beckn/Confirm.hs` |
| Driver assigned to ride | driver-app | `SharedLogic/Booking.hs` |
| Build on_confirm response | driver-app | `Beckn/ACL/OnConfirm.hs` |
| BAP receives on_confirm | rider-app | `Beckn/ACL/OnConfirm.hs` |
| BAP updates booking | rider-app | `Domain/Action/Beckn/OnConfirm.hs` |

**BookingStatus**: `CONFIRMED` → `TRIP_ASSIGNED`
**RideStatus**: `NEW`

## Phase 6: Ride In Progress

**Driver picks up rider and travels to destination.**

| Step | Service | Key File |
|------|---------|----------|
| Driver starts ride | driver-app | `Domain/Action/UI/Driver.hs` |
| Location updates streamed | driver-app | `Domain/Action/UI/Location.hs` |
| Location tracking | driver-app | `Lib/LocationUpdates.hs` |
| Real-time tracking (rider) | rider-app | `Domain/Action/UI/FollowRide.hs` |
| Toll detection | driver-app | `SharedLogic/TollsDetector.hs` |
| Blocked route detection | driver-app | `SharedLogic/BlockedRouteDetector.hs` |
| Build on_status updates | driver-app | `Beckn/ACL/OnStatus.hs` |
| BAP receives status updates | rider-app | `Beckn/ACL/OnStatus.hs` |
| Edit destination (rider) | rider-app | `Domain/Action/UI/EditLocation.hs` |
| Edit booking (driver side) | driver-app | `Domain/Action/UI/EditBooking.hs` |

**RideStatus**: `NEW` → `INPROGRESS`

## Phase 7: Ride Completion & Payment

**Driver ends ride. Fare finalized. Payment processed.**

| Step | Service | Key File |
|------|---------|----------|
| Driver ends ride | driver-app | `Domain/Action/UI/Driver.hs` |
| End ride internal logic | driver-app | `Domain/Action/UI/Ride/EndRide/Internal.hs` |
| Final fare calculated | driver-app | `SharedLogic/FareCalculator.hs` |
| Build on_update (fare) | driver-app | `Beckn/ACL/OnUpdate.hs` |
| BAP receives final fare | rider-app | `Beckn/ACL/OnUpdate.hs` |
| Payment initiated | rider-app | `Domain/Action/UI/Payment.hs` |
| Payment processing | rider-app | `SharedLogic/Payment.hs` |
| Juspay webhook | rider-app | `Domain/Action/UI/Payment.hs` (juspayWebhookHandler) |
| Driver fee computed | driver-app | `SharedLogic/DriverFee.hs` |
| Ride synced | driver-app | `SharedLogic/SyncRide.hs` |

**RideStatus**: `INPROGRESS` → `COMPLETED`
**BookingStatus**: `TRIP_ASSIGNED` → `COMPLETED`

## Phase 8: Rating & Feedback

**Rider rates the driver. Driver gets feedback.**

| Step | Service | Key File |
|------|---------|----------|
| Rider submits feedback | rider-app | `Domain/Action/UI/Feedback.hs` |
| Build BECKN rating | rider-app | `Beckn/ACL/Rating.hs` |
| BPP receives rating | driver-app | `Beckn/ACL/Rating.hs` |
| Rating stored | driver-app | `Domain/Action/UI/Rating.hs` |
| Feedback notification | driver-app | `SharedLogic/Allocator/Jobs/SendFeedbackPN.hs` |

## Cancellation Flow

Can happen at various stages:

| Step | Service | Key File |
|------|---------|----------|
| Rider cancels | rider-app | `Domain/Action/UI/Cancel.hs` |
| Cancellation logic | rider-app | `SharedLogic/Cancel.hs` |
| Build BECKN cancel | rider-app | `Beckn/ACL/Cancel.hs` |
| BPP receives cancel | driver-app | `Beckn/ACL/Cancel.hs` |
| Build on_cancel response | driver-app | `Beckn/ACL/OnCancel.hs` |
| BAP receives on_cancel | rider-app | `Beckn/ACL/OnCancel.hs` |
| Driver cancellation penalty | driver-app | `SharedLogic/DriverCancellationPenalty.hs` |
| Cancellation dues tracked | driver-app | `SharedLogic/UserCancellationDues.hs` |

**BookingStatus**: → `CANCELLED`
**RideStatus**: → `CANCELLED`

## Reassignment

When a driver cancels, the system can reassign:

| Step | Service | Key File |
|------|---------|----------|
| Booking awaits reassignment | rider-app | — |
| New search triggered | driver-app | `SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers.hs` |
| New driver found | driver-app | `SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle.hs` |

**BookingStatus**: `TRIP_ASSIGNED` → `AWAITING_REASSIGNMENT` → `REALLOCATED` → `TRIP_ASSIGNED`

## Related Docs

- BECKN protocol details: `05-beckn-protocol-flow.md`
- Status enums: `16-status-definitions.md`
- Rider app modules: `03-rider-app.md`
- Driver app modules: `04-driver-app.md`
