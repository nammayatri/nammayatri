# Status Definitions & State Transitions

## BookingStatus

**Location**: `lib/beckn-spec/src/Domain/Types/BookingStatus.hs`

Used by both BAP (rider-app) and BPP (driver-app) for tracking booking lifecycle.

### Values
| Status | Description |
|--------|-------------|
| `NEW` | Booking just created |
| `CONFIRMED` | Booking confirmed by BPP |
| `AWAITING_REASSIGNMENT` | Driver cancelled; waiting for new driver |
| `REALLOCATED` | New driver search initiated |
| `TRIP_ASSIGNED` | Driver assigned to the trip |
| `COMPLETED` | Trip completed successfully |
| `CANCELLED` | Booking cancelled by rider or system |

### State Transition Diagram

```
NEW ──────→ CONFIRMED ──────→ TRIP_ASSIGNED ──────→ COMPLETED
  │              │                    │
  │              │                    ↓
  │              │           AWAITING_REASSIGNMENT
  │              │                    │
  │              │                    ↓
  │              │              REALLOCATED ──→ TRIP_ASSIGNED
  │              │
  ↓              ↓
CANCELLED    CANCELLED                        CANCELLED
```

### Transitions
| From | To | Trigger |
|------|----|---------|
| `NEW` | `CONFIRMED` | BPP confirms booking |
| `NEW` | `CANCELLED` | Rider cancels before confirmation |
| `CONFIRMED` | `TRIP_ASSIGNED` | Driver assigned |
| `CONFIRMED` | `CANCELLED` | Cancellation before driver assignment |
| `TRIP_ASSIGNED` | `COMPLETED` | Ride completed successfully |
| `TRIP_ASSIGNED` | `AWAITING_REASSIGNMENT` | Driver cancels |
| `TRIP_ASSIGNED` | `CANCELLED` | Rider cancels after driver assigned |
| `AWAITING_REASSIGNMENT` | `REALLOCATED` | New search initiated |
| `REALLOCATED` | `TRIP_ASSIGNED` | New driver assigned |

## RideStatus

**Location**: `lib/beckn-spec/src/Domain/Types/RideStatus.hs`

Tracks the actual ride (vehicle movement) lifecycle.

### Values
| Status | Description |
|--------|-------------|
| `UPCOMING` | Ride scheduled for future |
| `NEW` | Ride created, driver assigned but not started |
| `INPROGRESS` | Driver has picked up rider, ride underway |
| `COMPLETED` | Ride finished successfully |
| `CANCELLED` | Ride cancelled |

### State Transition Diagram

```
UPCOMING ──→ NEW ──→ INPROGRESS ──→ COMPLETED
  │           │         │
  ↓           ↓         ↓
CANCELLED  CANCELLED  CANCELLED
```

### Transitions
| From | To | Trigger |
|------|----|---------|
| `UPCOMING` | `NEW` | Scheduled ride time arrives |
| `UPCOMING` | `CANCELLED` | Cancellation before ride time |
| `NEW` | `INPROGRESS` | Driver starts ride (picks up rider) |
| `NEW` | `CANCELLED` | Cancellation before pickup |
| `INPROGRESS` | `COMPLETED` | Driver ends ride |
| `INPROGRESS` | `CANCELLED` | Rare — emergency cancellation mid-ride |

## FRFSTicketBookingStatus

**Location**: `lib/beckn-spec/src/Domain/Types/FRFSTicketBookingStatus.hs`

Tracks public transport (metro/bus) ticket booking lifecycle.

### Values
| Status | Description |
|--------|-------------|
| `NEW` | Ticket booking initiated |
| `APPROVED` | Booking approved by operator |
| `PAYMENT_PENDING` | Waiting for payment |
| `CONFIRMING` | Payment received, confirming with operator |
| `CONFIRMED` | Ticket confirmed and issued |
| `FAILED` | Booking failed |
| `CANCELLED` | Booking cancelled by user |
| `COUNTER_CANCELLED` | Cancelled at counter |
| `CANCEL_INITIATED` | Cancellation in progress |
| `TECHNICAL_CANCEL_REJECTED` | Technical cancellation rejected by operator |

### State Transition Diagram

```
NEW ──→ APPROVED ──→ PAYMENT_PENDING ──→ CONFIRMING ──→ CONFIRMED
 │         │              │                  │              │
 ↓         ↓              ↓                  ↓              ↓
FAILED   FAILED        FAILED             FAILED    CANCEL_INITIATED
                                                         │
                                                    ┌────┴────┐
                                                    ↓         ↓
                                              CANCELLED   TECHNICAL_CANCEL_REJECTED
                                              COUNTER_CANCELLED
```

## Booking Type (Rider-App)

**Location**: `app/rider-platform/rider-app/Main/src-read-only/Domain/Types/Booking.hs`

Key fields: `id`, `riderId`, `quoteId`, `status` (BookingStatus), `providerId`, `fromLocation`, `toLocation`, `estimatedFare`, `estimatedDistance`, `paymentStatus`, `tripCategory`, `createdAt`, `updatedAt`

## Booking Type (Driver-App)

**Location**: `app/provider-platform/dynamic-offer-driver-app/Main/src-read-only/Domain/Types/Booking.hs`

Key fields: `id`, `transactionId`, `providerId`, `driverId`, `riderName`, `fromLocation`, `toLocation`, `estimatedFare`, `estimatedDistance`, `status`, `tripCategory`, `createdAt`

## SearchRequest (Rider-App)

**Location**: `app/rider-platform/rider-app/Main/src-read-only/Domain/Types/SearchRequest.hs`

Key fields: `id`, `riderId`, `fromLocation`, `toLocation`, `distance`, `distanceUnit`, `estimatedRideDuration`, `status`, `createdAt`

## How Status Maps to Ride Flow

| Phase | BookingStatus | RideStatus |
|-------|--------------|------------|
| Search | — | — |
| Offers received | — | — |
| Booking created | `NEW` | — |
| BPP confirms | `CONFIRMED` | — |
| Driver assigned | `TRIP_ASSIGNED` | `NEW` |
| Ride starts | `TRIP_ASSIGNED` | `INPROGRESS` |
| Ride ends | `COMPLETED` | `COMPLETED` |
| Rider cancels | `CANCELLED` | `CANCELLED` |
| Driver cancels | `AWAITING_REASSIGNMENT` | `CANCELLED` |
| Reassigned | `REALLOCATED` → `TRIP_ASSIGNED` | `NEW` (new ride) |

## Related Docs

- Ride flow: `06-ride-flow.md`
- BECKN protocol: `05-beckn-protocol-flow.md`
- FRFS details: `10-frfs-public-transport.md`
