# Namma Commute: Backend Implementation Plan (nammayatri repo)

## Overview

This document details the backend implementation plan for the Corporate Commute platform within the nammayatri monorepo. The backend uses Haskell, NammaDSL for code generation, BECKN protocol, and follows the existing BAP/BPP architecture.

**Key principle**: Extend existing modules and flows, minimize new standalone services.

**Path convention**: All paths in this document are relative to `Backend/` (i.e., `lib/...` means `Backend/lib/...`).

---

## 1. TripCategory Extension

### File: `lib/beckn-spec/src/Domain/Types/Trip.hs`

Add `Corporate CorporateMode` to the existing TripCategory ADT:

```haskell
data TripCategory
  = OneWay OneWayMode
  | Rental RentalMode
  | RideShare RideShareMode
  | InterCity OneWayMode (Maybe Text)
  | CrossCity OneWayMode (Maybe Text)
  | Ambulance OneWayMode
  | Delivery OneWayMode
  | Corporate CorporateMode   -- NEW

data CorporateMode
  = CorporateScheduled
  | CorporateOnDemand
  | CorporateShuttle
  | CorporatePooled
  | CorporateAirport
  | CorporateRental
  | CorporateIntercity
  deriving (Eq, Ord, Show, Read, Generic)
```

### File: `lib/beckn-spec/src/BecknV2/OnDemand/Enums.hs`

Add enums:
```haskell
-- Add to existing enum
| CORPORATE_SCHEDULED
| CORPORATE_ON_DEMAND
| CORPORATE_SHUTTLE
| CORPORATE_POOLED
```

### File: `lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs`

Add corporate BECKN tags:
```haskell
-- Corporate tags
corporateEntityIdTag = "corporate_entity_id"
corporateEmployeeIdTag = "corporate_employee_id"
corporateShiftIdTag = "corporate_shift_id"
corporateRouteIdTag = "corporate_route_id"
corporatePolicyIdTag = "corporate_policy_id"
corporateBillingModelTag = "corporate_billing_model"
```

### File: `lib/beckn-spec/src/BecknV2/OnDemand/Utils/Common.hs`

Add Corporate mapping in `tripCategoryToFulfillmentType` and `mkTripCategory`.

---

## 2. NammaDSL Storage Specs

All new specs go in `app/rider-platform/rider-app/Main/spec/Storage/`

### CorporateEntity.yaml

```yaml
imports:
  Merchant: Domain.Types.Merchant
  MerchantOperatingCity: Domain.Types.MerchantOperatingCity
  HighPrecMoney: Kernel.Types.Common
  Currency: Kernel.Types.Common

CorporateEntity:
  tableName: corporate_entity
  fields:
    id: Id CorporateEntity
    merchantId: Id Merchant
    merchantOperatingCityId: Id MerchantOperatingCity
    name: Text
    registeredName: Text
    gstin: Maybe Text
    industry: Text
    contactPersonName: Text
    contactEmail: Text
    contactPhone: Text
    billingAddress: Text
    billingModel: CorporateBillingModel
    billingCycleType: BillingCycleType
    creditLimit: HighPrecMoney
    currency: Currency
    status: CorporateEntityStatus
    contractStartDate: UTCTime
    contractEndDate: Maybe UTCTime
    walletBalance: HighPrecMoney
    safetyConfig: Maybe CorporateSafetyConfig
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    CorporateBillingModel:
      enum: "PER_TRIP | PER_EMPLOYEE_MONTH | PER_SEAT_KM | FLAT_ROUTE | HYBRID"
    BillingCycleType:
      enum: "WEEKLY | BIWEEKLY | MONTHLY"
    CorporateEntityStatus:
      enum: "ONBOARDING | ACTIVE | SUSPENDED | CHURNED"
    CorporateSafetyConfig:
      nightShiftEnabled: Bool
      womenSafetyRulesEnabled: Bool
      nightShiftStartHour: Int
      nightShiftEndHour: Int
      escortRequired: Bool
      ivrSafeDropEnabled: Bool
      derive: "Eq,Show,Read,Generic,ToJSON,FromJSON,ToSchema"

  queries:
    findByMerchantId:
      kvFunction: findAllWithKV
      where:
        merchantId: Id Merchant
    findByStatus:
      kvFunction: findAllWithKV
      where:
        status: CorporateEntityStatus
```

### CorporateEmployee.yaml

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  Person: Domain.Types.Person

CorporateEmployee:
  tableName: corporate_employee
  fields:
    id: Id CorporateEmployee
    corporateEntityId: Id CorporateEntity
    personId: Maybe (Id Person)
    employeeCode: Text
    name: Text
    email: Text
    phone: Text
    department: Text
    costCenter: Maybe Text
    gender: Gender
    defaultPickupLat: Double
    defaultPickupLon: Double
    defaultPickupAddress: Text
    reportingManagerEmail: Maybe Text
    status: CorporateEmployeeStatus
    linkedAt: Maybe UTCTime
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    CorporateEmployeeStatus:
      enum: "ACTIVE | INACTIVE | ON_LEAVE | TERMINATED"
    Gender:
      enum: "MALE | FEMALE | OTHER | PREFER_NOT_TO_SAY"

  queries:
    findByCorporateEntityId:
      kvFunction: findAllWithKV
      where:
        corporateEntityId: Id CorporateEntity
    findByPersonId:
      kvFunction: findOneWithKV
      where:
        personId: Maybe (Id Person)
    findByPhoneAndCorporateEntityId:
      kvFunction: findOneWithKV
      where:
        phone: Text
        corporateEntityId: Id CorporateEntity
```

### CorporateShift.yaml

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  ServiceTierType: Domain.Types.ServiceTierType

CorporateShift:
  tableName: corporate_shift
  fields:
    id: Id CorporateShift
    corporateEntityId: Id CorporateEntity
    name: Text
    pickupWindowStart: TimeOfDay
    pickupWindowEnd: TimeOfDay
    dropWindowStart: TimeOfDay
    dropWindowEnd: TimeOfDay
    activeDays: "[Int]"
    isNightShift: Bool
    maxDetourMinutes: Int
    maxOccupancy: Int
    allowedVehicleTiers: "[ServiceTierType]"
    status: ShiftStatus
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    ShiftStatus:
      enum: "ACTIVE | INACTIVE | ARCHIVED"
```

### CorporateRoute.yaml

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  CorporateShift: Domain.Types.CorporateShift
  ServiceTierType: Domain.Types.ServiceTierType

CorporateRoute:
  tableName: corporate_route
  fields:
    id: Id CorporateRoute
    corporateEntityId: Id CorporateEntity
    corporateShiftId: Id CorporateShift
    routeCode: Text
    direction: RouteDirection
    estimatedDurationMinutes: Int
    estimatedDistanceMeters: Int
    optimizedPolyline: Maybe Text
    vehicleTier: ServiceTierType
    maxCapacity: Int
    totalStops: Int
    status: RouteStatus
    lastOptimizedAt: Maybe UTCTime
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    RouteDirection:
      enum: "PICKUP | DROP"
    RouteStatus:
      enum: "ACTIVE | INACTIVE | OPTIMIZING | ARCHIVED"
```

### CorporateRouteStop.yaml

```yaml
imports:
  CorporateRoute: Domain.Types.CorporateRoute
  CorporateEmployee: Domain.Types.CorporateEmployee

CorporateRouteStop:
  tableName: corporate_route_stop
  fields:
    id: Id CorporateRouteStop
    corporateRouteId: Id CorporateRoute
    stopSequence: Int
    lat: Double
    lon: Double
    address: Text
    estimatedArrivalOffset: Int
    assignedEmployees: "[Id CorporateEmployee]"
    createdAt: UTCTime
    updatedAt: UTCTime
```

### CorporateRoster.yaml

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  CorporateEmployee: Domain.Types.CorporateEmployee
  CorporateShift: Domain.Types.CorporateShift
  CorporateRoute: Domain.Types.CorporateRoute
  CorporateRouteStop: Domain.Types.CorporateRouteStop

CorporateRoster:
  tableName: corporate_roster
  fields:
    id: Id CorporateRoster
    corporateEntityId: Id CorporateEntity
    corporateEmployeeId: Id CorporateEmployee
    corporateShiftId: Id CorporateShift
    corporateRouteId: Maybe (Id CorporateRoute)
    corporateRouteStopId: Maybe (Id CorporateRouteStop)
    rosterDate: Day
    attendanceStatus: AttendanceStatus
    bookingId: Maybe (Id Booking)
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    AttendanceStatus:
      enum: "SCHEDULED | CONFIRMED | ON_LEAVE | NO_SHOW | COMPLETED | CANCELLED"
```

### CorporatePolicy.yaml

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  ServiceTierType: Domain.Types.ServiceTierType
  HighPrecMoney: Kernel.Types.Common

CorporatePolicy:
  tableName: corporate_policy
  fields:
    id: Id CorporatePolicy
    corporateEntityId: Id CorporateEntity
    name: Text
    policyType: PolicyType
    maxFarePerTrip: Maybe HighPrecMoney
    maxMonthlyBudgetPerEmployee: Maybe HighPrecMoney
    allowedTripCategories: "[Text]"
    allowedServiceTiers: "[ServiceTierType]"
    requiresApproval: Bool
    approvalThreshold: Maybe HighPrecMoney
    nightShiftSafetyEnabled: Bool
    nightShiftStartHour: Maybe Int
    nightShiftEndHour: Maybe Int
    womenSafetyRulesEnabled: Bool
    surgeCap: Maybe Double
    status: PolicyStatus
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    PolicyType:
      enum: "RIDE_BUDGET | VEHICLE_TIER | APPROVAL | SAFETY | COMBINED"
    PolicyStatus:
      enum: "ACTIVE | INACTIVE | ARCHIVED"
```

### CorporateWallet.yaml

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  HighPrecMoney: Kernel.Types.Common
  Currency: Kernel.Types.Common

CorporateWallet:
  tableName: corporate_wallet
  fields:
    id: Id CorporateWallet
    corporateEntityId: Id CorporateEntity
    balance: HighPrecMoney
    currency: Currency
    minBalanceThreshold: HighPrecMoney
    autoTopUpEnabled: Bool
    autoTopUpAmount: Maybe HighPrecMoney
    lastTopUpAt: Maybe UTCTime
    status: WalletStatus
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    WalletStatus:
      enum: "ACTIVE | FROZEN | CLOSED"
```

### CorporateInvoice.yaml

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  HighPrecMoney: Kernel.Types.Common
  Currency: Kernel.Types.Common

CorporateInvoice:
  tableName: corporate_invoice
  fields:
    id: Id CorporateInvoice
    corporateEntityId: Id CorporateEntity
    invoiceNumber: Text
    periodStart: UTCTime
    periodEnd: UTCTime
    totalTrips: Int
    totalAmount: HighPrecMoney
    taxAmount: HighPrecMoney
    netAmount: HighPrecMoney
    currency: Currency
    status: InvoiceStatus
    pdfUrl: Maybe Text
    generatedAt: UTCTime
    paidAt: Maybe UTCTime
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    InvoiceStatus:
      enum: "DRAFT | PENDING_APPROVAL | APPROVED | SENT | PAID | DISPUTED | OVERDUE"
```

### CorporateSupplyPool.yaml (driver-app spec)

**Path**: `app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/CorporateSupplyPool.yaml`

```yaml
imports:
  Person: Domain.Types.Person

CorporateSupplyPool:
  tableName: corporate_supply_pool
  fields:
    id: Id CorporateSupplyPool
    corporateEntityId: Text  -- ID from rider-app schema
    driverId: Id Person
    qualityTier: QualityTier
    minRating: Double
    isActive: Bool
    addedAt: UTCTime
    lastVerifiedAt: Maybe UTCTime
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    QualityTier:
      enum: "STANDARD | PREMIUM | EXECUTIVE"
```

---

## 3. Booking Flow Integration

### Modify: `app/rider-platform/rider-app/Main/spec/Storage/Booking.yaml`

Add optional corporate fields:
```yaml
  # Add to existing fields
  corporateEntityId: Maybe (Id CorporateEntity)
  corporateEmployeeId: Maybe (Id CorporateEmployee)
  corporateShiftId: Maybe (Id CorporateShift)
  corporateRouteId: Maybe (Id CorporateRoute)
  corporatePolicyId: Maybe (Id CorporatePolicy)
```

### Modify: `app/rider-platform/rider-app/Main/spec/Storage/SearchRequest.yaml`

Add:
```yaml
  corporateEntityId: Maybe (Id CorporateEntity)
  corporateMode: Maybe Text
```

### New API Spec: `app/rider-platform/rider-app/Main/spec/API/CorporateBooking.yaml`

```yaml
module: Domain.Action.UI.CorporateBooking

imports:
  CorporateEntity: Domain.Types.CorporateEntity
  CorporateEmployee: Domain.Types.CorporateEmployee
  CorporateShift: Domain.Types.CorporateShift

apis:
  # Schedule corporate rides from roster
  - POST:
      endpoint: /corporate/{corporateEntityId}/schedule
      auth: DashboardAuth
      request:
        type: ScheduleCorporateRidesReq
      response:
        type: ScheduleCorporateRidesResp

  # Employee books on-demand corporate ride
  - POST:
      endpoint: /corporate/ride/ondemand
      auth: TokenAuth
      request:
        type: CorporateOnDemandBookingReq
      response:
        type: CorporateOnDemandBookingResp

  # Get employee's upcoming corporate rides
  - GET:
      endpoint: /corporate/rides/upcoming
      auth: TokenAuth
      response:
        type: "[CorporateRideInfo]"

  # Confirm attendance for scheduled ride
  - POST:
      endpoint: /corporate/ride/{rideId}/confirm-attendance
      auth: TokenAuth
      response:
        type: APISuccess

  # Mark leave (cancel scheduled ride)
  - POST:
      endpoint: /corporate/ride/{rideId}/mark-leave
      auth: TokenAuth
      response:
        type: APISuccess
```

---

## 4. Key Business Logic Modules

### New: `SharedLogic/CorporateRouteOptimizer.hs`

Route optimization engine using existing OSRM/Maps integration:

```
Algorithm:
1. Input: List of employee addresses + office location + shift time + vehicle capacity
2. Stop clustering: Group nearby employees into pickup stops (K-means on lat/lon)
3. Route sequencing: Solve TSP variant with time windows and capacity constraints
4. Gender safety: Ensure no female first-pickup/last-drop in night shifts
5. Multi-route generation: If employees exceed single vehicle capacity, split into routes
6. Output: Optimized routes with stops, sequence, timing, assigned vehicles
```

Reuses:
- `Tools/Maps.hs` -- getDistance, getRoutes
- `lib/location-updates` -- distance calculations
- `SharedLogic/Serviceability.hs` -- area checks

### New: `SharedLogic/CorporateScheduler.hs`

Generates bookings from daily roster:

```
Algorithm:
1. Input: Active roster entries for next day + routes + shifts
2. For each route: Create a SearchRequest with Corporate TripCategory
3. Schedule via existing scheduler library (lib/scheduler)
4. Allocator picks up and matches to curated supply pool
5. Notify assigned drivers and employees
```

Reuses:
- `lib/scheduler/src/Lib/Scheduler/ScheduleJob.hs` -- job scheduling
- `SharedLogic/Allocator/Jobs/ScheduledRides/` -- scheduled ride framework

### New: `SharedLogic/CorporateBilling.hs`

Corporate billing logic:

```
Functions:
- calculateTripCost: GPS-km based fare OR flat route rate OR policy cap
- generateTripSheet: Daily trip sheet with all completed rides
- reconcileBilling: Match trips to corporate policy, flag anomalies
- generateInvoice: Monthly consolidated invoice
- debitCorporateWallet: Real-time wallet debit per trip
- makerCheckerApproval: Two-level approval for disputed trips
```

Reuses:
- `SharedLogic/FareCalculator.hs` -- base fare calculation
- `lib/payment/` -- Juspay integration
- `lib/finance-kernel/` -- HighPrecMoney, currency handling
- `Domain/Action/UI/Invoice.hs` -- invoice generation

### Modify: `SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/DriverPool.hs`

Add corporate supply pool filter:

```
Changes:
- Add CorporateSupplyPool filter: only include drivers in corporate's curated pool
- Add quality tier filter: min rating, max vehicle age, verified BGC
- Add vehicle tier filter: match corporate policy's allowed tiers
- Priority: corporate pool drivers get first priority for corporate rides
```

### Modify: `app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/FareCalculator.hs` (BPP side)

**Note**: FareCalculator is on the BPP (driver-app) side, not rider-app.

Add corporate fare modes:

```
Changes:
- FLAT_ROUTE: Fixed fare per route regardless of distance
- PER_KM_CAPPED: Per-km fare with corporate-negotiated cap
- SURGE_CAPPED: Apply corporate surge cap (e.g., max 1.2x)
- POLICY_OVERRIDE: Apply corporate policy fare override
```

### Ownership Note: Routing & Billing Code Split

| Logic | shared-kernel (low-level) | rider-app (orchestrator) |
|-------|--------------------------|-------------------------|
| **Routing** | `Kernel/Utils/CorporateRouting.hs`: TSP solver, stop clustering algorithm, distance matrix utils | `SharedLogic/CorporateRouteOptimizer.hs`: Full pipeline orchestration, maps API calls, gender safety, multi-route splitting |
| **Billing** | `Kernel/Utils/CorporateBilling.hs`: Fare calculation primitives, GPS-km formulas, maker-checker validation | `SharedLogic/CorporateBilling.hs`: Trip-sheet generation, invoice lifecycle, wallet debit orchestration, Juspay integration |

---

## 5. Dashboard API Integration

### New: `app/dashboard/rider-dashboard/spec/API/Corporate.yaml`

Dashboard APIs for corporate management with DashboardAuth.

### New: `app/dashboard/provider-dashboard/spec/API/CorporateFleet.yaml`

Fleet management APIs for corporate supply pool.

### New: `app/dashboard/CommonAPIs/spec/API/CorporateTypes.yaml`

Shared corporate API types.

---

## 6. Database Migrations

All migrations go in `dev/migrations/rider-app/`:

```
0001_create_corporate_entity.sql
0002_create_corporate_employee.sql
0003_create_corporate_shift.sql
0004_create_corporate_route.sql
0005_create_corporate_route_stop.sql
0006_create_corporate_policy.sql
0007_create_corporate_roster.sql
0008_create_corporate_wallet.sql
0009_create_corporate_invoice.sql
0010_alter_booking_add_corporate_fields.sql
0011_alter_search_request_add_corporate_fields.sql
0012_alter_ride_add_corporate_fields.sql
0013_alter_person_add_corporate_employee_ref.sql
```

Driver-side migrations in `dev/migrations/dynamic-offer-driver-app/`:
```
0001_create_corporate_supply_pool.sql
0002_create_corporate_driver_quality_metrics.sql
```

---

## 7. Configuration (dhall-configs)

Add to `dhall-configs/dev/`:
- Corporate-specific merchant config
- Corporate fare policy defaults
- Corporate allocation config
- Women safety rule config per city

---

## 8. Kafka Events

New Kafka topics for corporate event streaming:
- `corporate-trip-completed` -- trip sheet events
- `corporate-billing-event` -- billing/wallet events
- `corporate-safety-event` -- safety incidents
- `corporate-compliance-alert` -- compliance document expiry
- `corporate-roster-change` -- roster updates

Reuse existing Kafka infrastructure (`lib/producer/`, `app/kafka-consumers/`).

---

## 9. Implementation Sequence

### Sprint 1-2 (Weeks 1-4): Foundation
- [ ] Add CorporateMode to TripCategory
- [ ] Create all NammaDSL storage specs
- [ ] Run code generator, fix compilation
- [ ] Create database migrations
- [ ] Implement CorporateEntity CRUD
- [ ] Implement CorporateEmployee CRUD + bulk upload

### Sprint 3-4 (Weeks 5-8): Shift & Route Management
- [ ] Implement CorporateShift CRUD
- [ ] Build CorporateRouteOptimizer (stop clustering + TSP)
- [ ] Implement CorporateRoute + CorporateRouteStop CRUD
- [ ] Build route optimization API endpoint
- [ ] Dashboard APIs for shift and route management

### Sprint 5-6 (Weeks 9-12): Booking Flow
- [ ] Modify Search.hs for corporate context
- [ ] Modify Confirm.hs for corporate policy validation
- [ ] Build CorporateScheduler (roster-to-booking)
- [ ] Implement curated supply pool
- [ ] Modify DriverPool for corporate filters
- [ ] Add corporate fare modes to FareCalculator

### Sprint 7-8 (Weeks 13-16): Billing & Safety
- [ ] Build CorporateBilling (trip-sheet, reconciliation, invoicing)
- [ ] Implement CorporateWallet (Juspay integration)
- [ ] Build women safety routing constraints
- [ ] Safe-drop confirmation flow
- [ ] Invoice generation
- [ ] Build exception handling: replacement dispatch (F10a)
- [ ] Build basic ops desk APIs: incident creation, manual dispatch override (F10b)

### Sprint 9-10 (Weeks 17-20): Dashboard APIs, Analytics & Polish
- [ ] Dashboard APIs for all corporate features
- [ ] Build `CorporateAnalytics.yaml` API spec -- analytics queries from ClickHouse
- [ ] Build `CorporateSafety.yaml` API spec -- safety/compliance status queries
- [ ] Build `CorporateEmployeeUI.yaml` API spec -- employee-facing endpoints (profile, current shift, schedule)
- [ ] Kafka event pipeline for corporate events
- [ ] ClickHouse analytics events for corporate trips
- [ ] Dead-km tracking analytics (F10d)
- [ ] Driver welfare/training record tracking (F10c)
- [ ] API integration testing
- [ ] Performance optimization
