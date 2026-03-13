# Namma Commute: Backend Implementation Plan (nammayatri repo)

## Overview

This document details the backend implementation plan for the Corporate Commute platform within the nammayatri monorepo. The backend uses Haskell, NammaDSL for code generation, BECKN protocol, and follows the existing BAP/BPP architecture.

**Key principle**: Extend existing modules and flows, minimize new standalone services.

**Path convention**: All paths in this document are relative to `Backend/` (i.e., `lib/...` means `Backend/lib/...`).

---

## 3-Loop Development Process

All corporate-commute features follow a maker-checker workflow across three loops:

**Loop 1 -- Design**
- **Maker** writes the NammaDSL YAML spec (storage or API).
- **Checker** validates that the spec compiles (`cabal build all`), naming conventions are followed, constraints/indexes are correct, and types are consistent with existing domain models.
- Exit criteria: spec compiles cleanly, checker approves PR.

**Loop 2 -- Code**
- **Maker** implements business logic in `Domain/Action/UI/`, `SharedLogic/`, and extra query files.
- **Reviewer** verifies: test coverage, query performance (EXPLAIN on new queries), correct RBAC (DashboardAuth vs TokenAuth), error handling with `fromMaybeM`, no direct SQL (use generated queries), and wallet/billing arithmetic uses `HighPrecMoney`.
- Exit criteria: all builds pass, reviewer approves PR.

**Loop 3 -- Test**
- **Creator** writes integration tests covering happy path, edge cases, and RBAC boundaries.
- **Tester** validates: coverage of all status transitions, boundary conditions (zero balance, expired contract, overlapping shifts), concurrency (double-debit, race on roster confirm), and safety rule enforcement.
- Exit criteria: test suite green, tester approves PR.

---

## 1. Corporate Context via Tags (No TripCategory Changes)

### Design Decision

Corporate bookings do NOT add a new variant to the `TripCategory` ADT. Instead, corporate context is carried as an optional `CorporateBookingContext` record attached to `SearchRequest` and `Booking`. On the BECKN wire, this context travels via `fulfillment.tags`.

This avoids:
- Modifying the heavily-used `Trip.hs` TripCategory ADT
- Adding new `FulfillmentType` enums to BECKN
- Branching every pattern match on TripCategory across the codebase

A corporate ride is still a `OneWay`, `Rental`, `RideShare`, etc. -- the corporate context is orthogonal metadata.

### CorporateBookingContext Record

Defined in rider-app domain types (generated from NammaDSL or as an extra domain type):

```haskell
data CorporateBookingContext = CorporateBookingContext
  { corporateEntityId    :: Id CorporateEntity
  , corporateEmployeeId  :: Id CorporateEmployee
  , corporateShiftId     :: Maybe (Id CorporateShift)
  , corporateRouteId     :: Maybe (Id CorporateRoute)
  , corporatePolicyId    :: Maybe (Id CorporatePolicy)
  , billingModel         :: CorporateBillingModel
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
```

### BECKN Transport: fulfillment.tags

Tags are defined in `lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs`:

```haskell
-- Corporate tags (carried in fulfillment.tags)
corporateEntityIdTag :: Text
corporateEntityIdTag = "corporate_entity_id"

corporateEmployeeIdTag :: Text
corporateEmployeeIdTag = "corporate_employee_id"

corporateShiftIdTag :: Text
corporateShiftIdTag = "corporate_shift_id"

corporateRouteIdTag :: Text
corporateRouteIdTag = "corporate_route_id"

corporatePolicyIdTag :: Text
corporatePolicyIdTag = "corporate_policy_id"

corporateBillingModelTag :: Text
corporateBillingModelTag = "corporate_billing_model"
```

### ACL Layer

In `Beckn/ACL/OnDemand/Search.hs` (and corresponding `OnSearch`, `Confirm` ACLs):
- **Outbound**: If `SearchRequest` has a `CorporateBookingContext`, serialize each field as a tag in the `fulfillment.tags` list.
- **Inbound**: Parse `fulfillment.tags` back into `CorporateBookingContext` and attach to the domain object.

No changes to `Trip.hs`, no new `FulfillmentType` enums, no changes to `tripCategoryToFulfillmentType` or `mkTripCategory`.

---

## 2. NammaDSL Storage Specs

All new specs go in `app/rider-platform/rider-app/Main/spec/Storage/` unless otherwise noted.

### NammaDSL Conventions Used

- **Enums**: comma-separated -- `enum: "VALUE1,VALUE2,VALUE3"` (no pipes)
- **Enum derive**: `derive': "HttpInstance"` for enums appearing in API responses
- **Constraints**: every spec includes `constraints:` with `PrimaryKey` and relevant `SecondaryKey`
- **Extra operations**: every spec includes `extraOperations:` with at least `EXTRA_QUERY_FILE`
- **Query where clauses**: use plain field names (e.g., `merchantId: merchantId`), not typed signatures
- **Imports**: use full module paths; common types (`Text`, `Maybe`, `Int`, `Bool`, `Id`, `UTCTime`, `HighPrecMoney`, `Currency`) are auto-imported

### CorporateEntity.yaml (full corrected example)

```yaml
imports:
  Merchant: Domain.Types.Merchant
  MerchantOperatingCity: Domain.Types.MerchantOperatingCity
  PartnerOrganization: Domain.Types.PartnerOrganization

CorporateEntity:
  tableName: corporate_entity

  fields:
    id: Id CorporateEntity
    merchantId: Id Merchant
    merchantOperatingCityId: Id MerchantOperatingCity
    partnerOrgId: Maybe (Id PartnerOrganization)
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
    safetyConfig: Maybe CorporateSafetyConfig
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    CorporateBillingModel:
      enum: "PER_TRIP,PER_EMPLOYEE_MONTH,PER_SEAT_KM,FLAT_ROUTE,HYBRID"
      derive': "HttpInstance"
    BillingCycleType:
      enum: "WEEKLY,BIWEEKLY,MONTHLY"
      derive': "HttpInstance"
    CorporateEntityStatus:
      enum: "ONBOARDING,ACTIVE,SUSPENDED,CHURNED"
      derive': "HttpInstance"
    CorporateSafetyConfig:
      nightShiftEnabled: Bool
      womenSafetyRulesEnabled: Bool
      nightShiftStartHour: Int
      nightShiftEndHour: Int
      escortRequired: Bool
      ivrSafeDropEnabled: Bool
      derive: "Eq,Show,Read,Generic,ToJSON,FromJSON,ToSchema"

  constraints:
    id: PrimaryKey
    merchantId: SecondaryKey
    partnerOrgId: SecondaryKey

  queries:
    findByMerchantId:
      kvFunction: findAllWithKV
      where:
        merchantId: merchantId
    findByStatus:
      kvFunction: findAllWithKV
      where:
        status: status
    findByPartnerOrgId:
      kvFunction: findAllWithKV
      where:
        partnerOrgId: partnerOrgId

  extraOperations:
    - EXTRA_QUERY_FILE
    - EXTRA_DOMAIN_TYPE_FILE
```

### CorporateEmployee.yaml (full corrected example)

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  Person: Domain.Types.Person
  Gender: Domain.Types.Person

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
      enum: "ACTIVE,INACTIVE,ON_LEAVE,TERMINATED"
      derive': "HttpInstance"

  constraints:
    id: PrimaryKey
    corporateEntityId: SecondaryKey
    personId: SecondaryKey
    phone: SecondaryKey

  queries:
    findByCorporateEntityId:
      kvFunction: findAllWithKV
      where:
        corporateEntityId: corporateEntityId
    findByPersonId:
      kvFunction: findOneWithKV
      where:
        personId: personId
    findByPhoneAndCorporateEntityId:
      kvFunction: findOneWithKV
      where:
        phone: phone
        corporateEntityId: corporateEntityId

  extraOperations:
    - EXTRA_QUERY_FILE
    - EXTRA_DOMAIN_TYPE_FILE
```

### CorporateRoster.yaml (full corrected example)

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  CorporateEmployee: Domain.Types.CorporateEmployee
  CorporateShift: Domain.Types.CorporateShift
  CorporateRoute: Domain.Types.CorporateRoute
  CorporateRouteStop: Domain.Types.CorporateRouteStop
  Booking: Domain.Types.Booking

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
    replacementBookingId: Maybe (Id Booking)
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    AttendanceStatus:
      enum: "SCHEDULED,CONFIRMED,ON_LEAVE,NO_SHOW,COMPLETED,CANCELLED,VEHICLE_BREAKDOWN"
      derive': "HttpInstance"

  constraints:
    id: PrimaryKey
    corporateEntityId: SecondaryKey
    corporateEmployeeId: SecondaryKey
    rosterDate: SecondaryKey

  queries:
    findByCorporateEntityIdAndDate:
      kvFunction: findAllWithKV
      where:
        corporateEntityId: corporateEntityId
        rosterDate: rosterDate
    findByCorporateEmployeeIdAndDate:
      kvFunction: findOneWithKV
      where:
        corporateEmployeeId: corporateEmployeeId
        rosterDate: rosterDate
    updateAttendanceStatus:
      kvFunction: updateWithKV
      params:
        - attendanceStatus
      where:
        id: id
    updateReplacementBookingId:
      kvFunction: updateWithKV
      params:
        - replacementBookingId
      where:
        id: id

  extraOperations:
    - EXTRA_QUERY_FILE
```

### CorporateInvoice.yaml (full corrected example)

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity

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
    cgstAmount: HighPrecMoney
    sgstAmount: HighPrecMoney
    igstAmount: HighPrecMoney
    netAmount: HighPrecMoney
    currency: Currency
    sacCode: Text
    placeOfSupply: Text
    supplierGstin: Text
    recipientGstin: Maybe Text
    eInvoiceIrn: Maybe Text
    status: InvoiceStatus
    pdfUrl: Maybe Text
    generatedAt: UTCTime
    paidAt: Maybe UTCTime
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    InvoiceStatus:
      enum: "DRAFT,PENDING_APPROVAL,APPROVED,SENT,PAID,DISPUTED,OVERDUE,CREDITED"
      derive': "HttpInstance"

  constraints:
    id: PrimaryKey
    corporateEntityId: SecondaryKey
    invoiceNumber: SecondaryKey
    status: SecondaryKey

  queries:
    findByCorporateEntityId:
      kvFunction: findAllWithKV
      where:
        corporateEntityId: corporateEntityId
    findByInvoiceNumber:
      kvFunction: findOneWithKV
      where:
        invoiceNumber: invoiceNumber
    findByStatus:
      kvFunction: findAllWithKV
      where:
        status: status
    updateStatus:
      kvFunction: updateWithKV
      params:
        - status
      where:
        id: id

  extraOperations:
    - EXTRA_QUERY_FILE
```

### Other Storage Specs (summary with key fields)

All follow the same conventions (comma-separated enums, constraints, extraOperations, plain field names in queries).

**CorporateShift.yaml**: Same fields as before. Add `constraints: { id: PrimaryKey, corporateEntityId: SecondaryKey }`, `extraOperations: [EXTRA_QUERY_FILE]`. Fix enum syntax to comma-separated.

**CorporateRoute.yaml**: Same fields as before. Add `constraints: { id: PrimaryKey, corporateEntityId: SecondaryKey, corporateShiftId: SecondaryKey }`, `extraOperations: [EXTRA_QUERY_FILE]`. Fix enum syntax.

**CorporateRouteStop.yaml**: Remove `assignedEmployees: "[Id CorporateEmployee]"` array field. Assignments are tracked via the `CorporateRouteStopAssignment` junction table (below). Add `constraints: { id: PrimaryKey, corporateRouteId: SecondaryKey }`, `extraOperations: [EXTRA_QUERY_FILE]`.

**CorporatePolicy.yaml**: Same fields. Add `constraints: { id: PrimaryKey, corporateEntityId: SecondaryKey }`, `extraOperations: [EXTRA_QUERY_FILE]`. Fix enum syntax.

**CorporateWallet.yaml**: Same fields. Add `constraints: { id: PrimaryKey, corporateEntityId: SecondaryKey }`, `extraOperations: [EXTRA_QUERY_FILE]`. Fix enum syntax.

**CorporateSupplyPool.yaml** (driver-app): Same fields. Path: `app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/CorporateSupplyPool.yaml`. Add `constraints: { id: PrimaryKey, driverId: SecondaryKey, corporateEntityId: SecondaryKey }`, `extraOperations: [EXTRA_QUERY_FILE]`. Fix enum syntax.

### New Entities

**CorporateRouteStopAssignment.yaml** -- Junction table replacing the `assignedEmployees` array on CorporateRouteStop:

```yaml
imports:
  CorporateRouteStop: Domain.Types.CorporateRouteStop
  CorporateEmployee: Domain.Types.CorporateEmployee

CorporateRouteStopAssignment:
  tableName: corporate_route_stop_assignment

  fields:
    id: Id CorporateRouteStopAssignment
    corporateRouteStopId: Id CorporateRouteStop
    corporateEmployeeId: Id CorporateEmployee
    boardingOrder: Int
    createdAt: UTCTime
    updatedAt: UTCTime

  constraints:
    id: PrimaryKey
    corporateRouteStopId: SecondaryKey
    corporateEmployeeId: SecondaryKey

  queries:
    findByRouteStopId:
      kvFunction: findAllWithKV
      where:
        corporateRouteStopId: corporateRouteStopId
    findByEmployeeId:
      kvFunction: findAllWithKV
      where:
        corporateEmployeeId: corporateEmployeeId

  extraOperations:
    - EXTRA_QUERY_FILE
```

**CorporateHolidayCalendar.yaml**:

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity

CorporateHolidayCalendar:
  tableName: corporate_holiday_calendar

  fields:
    id: Id CorporateHolidayCalendar
    corporateEntityId: Id CorporateEntity
    holidayDate: Day
    name: Text
    isOptional: Bool
    createdAt: UTCTime
    updatedAt: UTCTime

  constraints:
    id: PrimaryKey
    corporateEntityId: SecondaryKey

  queries:
    findByCorporateEntityId:
      kvFunction: findAllWithKV
      where:
        corporateEntityId: corporateEntityId
    findByCorporateEntityIdAndDate:
      kvFunction: findOneWithKV
      where:
        corporateEntityId: corporateEntityId
        holidayDate: holidayDate

  extraOperations:
    - EXTRA_QUERY_FILE
```

**CorporateApprovalRequest.yaml**:

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  CorporateEmployee: Domain.Types.CorporateEmployee
  CorporatePolicy: Domain.Types.CorporatePolicy
  Booking: Domain.Types.Booking
  Person: Domain.Types.Person

CorporateApprovalRequest:
  tableName: corporate_approval_request

  fields:
    id: Id CorporateApprovalRequest
    corporateEntityId: Id CorporateEntity
    corporateEmployeeId: Id CorporateEmployee
    corporatePolicyId: Id CorporatePolicy
    bookingId: Maybe (Id Booking)
    requestType: ApprovalRequestType
    estimatedFare: HighPrecMoney
    currency: Currency
    reason: Text
    approverEmail: Text
    approvedById: Maybe (Id Person)
    status: ApprovalStatus
    decidedAt: Maybe UTCTime
    expiresAt: UTCTime
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    ApprovalRequestType:
      enum: "OVER_BUDGET,OFF_POLICY_TIER,OFF_HOURS,MANUAL"
      derive': "HttpInstance"
    ApprovalStatus:
      enum: "PENDING,APPROVED,REJECTED,EXPIRED,AUTO_APPROVED"
      derive': "HttpInstance"

  constraints:
    id: PrimaryKey
    corporateEntityId: SecondaryKey
    corporateEmployeeId: SecondaryKey
    status: SecondaryKey

  queries:
    findByCorporateEntityIdAndStatus:
      kvFunction: findAllWithKV
      where:
        corporateEntityId: corporateEntityId
        status: status
    findByEmployeeIdAndStatus:
      kvFunction: findAllWithKV
      where:
        corporateEmployeeId: corporateEmployeeId
        status: status
    updateStatus:
      kvFunction: updateWithKV
      params:
        - status
        - approvedById
        - decidedAt
      where:
        id: id

  extraOperations:
    - EXTRA_QUERY_FILE
```

**CorporateRideFeedback.yaml**:

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  CorporateEmployee: Domain.Types.CorporateEmployee
  Booking: Domain.Types.Booking

CorporateRideFeedback:
  tableName: corporate_ride_feedback

  fields:
    id: Id CorporateRideFeedback
    corporateEntityId: Id CorporateEntity
    corporateEmployeeId: Id CorporateEmployee
    bookingId: Id Booking
    rating: Int
    feedbackText: Maybe Text
    safetyRating: Maybe Int
    punctualityRating: Maybe Int
    cleanlinessRating: Maybe Int
    createdAt: UTCTime

  constraints:
    id: PrimaryKey
    bookingId: SecondaryKey
    corporateEntityId: SecondaryKey

  queries:
    findByBookingId:
      kvFunction: findOneWithKV
      where:
        bookingId: bookingId
    findByCorporateEntityId:
      kvFunction: findAllWithKV
      where:
        corporateEntityId: corporateEntityId

  extraOperations:
    - EXTRA_QUERY_FILE
```

**CorporateSafetyIncident.yaml**:

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  CorporateEmployee: Domain.Types.CorporateEmployee
  Booking: Domain.Types.Booking
  Person: Domain.Types.Person

CorporateSafetyIncident:
  tableName: corporate_safety_incident

  fields:
    id: Id CorporateSafetyIncident
    corporateEntityId: Id CorporateEntity
    corporateEmployeeId: Id CorporateEmployee
    bookingId: Id Booking
    driverId: Maybe Text
    incidentType: SafetyIncidentType
    severity: IncidentSeverity
    description: Text
    gpsLat: Maybe Double
    gpsLon: Maybe Double
    reportedAt: UTCTime
    resolvedAt: Maybe UTCTime
    resolvedById: Maybe (Id Person)
    status: IncidentStatus
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    SafetyIncidentType:
      enum: "SOS_TRIGGERED,ROUTE_DEVIATION,UNSCHEDULED_STOP,NIGHT_SAFETY_VIOLATION,DRIVER_BEHAVIOR,OTHER"
      derive': "HttpInstance"
    IncidentSeverity:
      enum: "LOW,MEDIUM,HIGH,CRITICAL"
      derive': "HttpInstance"
    IncidentStatus:
      enum: "REPORTED,INVESTIGATING,RESOLVED,ESCALATED"
      derive': "HttpInstance"

  constraints:
    id: PrimaryKey
    corporateEntityId: SecondaryKey
    bookingId: SecondaryKey
    status: SecondaryKey

  queries:
    findByCorporateEntityIdAndStatus:
      kvFunction: findAllWithKV
      where:
        corporateEntityId: corporateEntityId
        status: status
    findByBookingId:
      kvFunction: findOneWithKV
      where:
        bookingId: bookingId
    updateStatus:
      kvFunction: updateWithKV
      params:
        - status
        - resolvedAt
        - resolvedById
      where:
        id: id

  extraOperations:
    - EXTRA_QUERY_FILE
```

**CorporateCreditNote.yaml**:

```yaml
imports:
  CorporateEntity: Domain.Types.CorporateEntity
  CorporateInvoice: Domain.Types.CorporateInvoice

CorporateCreditNote:
  tableName: corporate_credit_note

  fields:
    id: Id CorporateCreditNote
    corporateEntityId: Id CorporateEntity
    corporateInvoiceId: Id CorporateInvoice
    creditNoteNumber: Text
    amount: HighPrecMoney
    cgstAmount: HighPrecMoney
    sgstAmount: HighPrecMoney
    igstAmount: HighPrecMoney
    currency: Currency
    reason: Text
    status: CreditNoteStatus
    issuedAt: UTCTime
    createdAt: UTCTime
    updatedAt: UTCTime

  types:
    CreditNoteStatus:
      enum: "DRAFT,ISSUED,APPLIED,CANCELLED"
      derive': "HttpInstance"

  constraints:
    id: PrimaryKey
    corporateEntityId: SecondaryKey
    corporateInvoiceId: SecondaryKey
    creditNoteNumber: SecondaryKey

  queries:
    findByCorporateEntityId:
      kvFunction: findAllWithKV
      where:
        corporateEntityId: corporateEntityId
    findByCorporateInvoiceId:
      kvFunction: findAllWithKV
      where:
        corporateInvoiceId: corporateInvoiceId

  extraOperations:
    - EXTRA_QUERY_FILE
```

---

## 3. Booking Flow Integration

### Approach: CorporateBookingContext as Optional Fields

Corporate rides use existing `TripCategory` values (e.g., `OneWay OneWayOnDemand`). The corporate context is attached as optional fields on `SearchRequest` and `Booking`.

### Modify: `app/rider-platform/rider-app/Main/spec/Storage/SearchRequest.yaml`

Add optional corporate context fields:
```yaml
  # Add to existing fields section
  corporateEntityId: Maybe Text
  corporateEmployeeId: Maybe Text
  corporateShiftId: Maybe Text
  corporateRouteId: Maybe Text
  corporatePolicyId: Maybe Text
  corporateBillingModel: Maybe Text
```

Using `Text` (not `Id` types) to avoid import coupling; the IDs are validated at the business logic layer when constructing `CorporateBookingContext`.

### Modify: `app/rider-platform/rider-app/Main/spec/Storage/Booking.yaml`

Add the same optional fields:
```yaml
  # Add to existing fields section
  corporateEntityId: Maybe Text
  corporateEmployeeId: Maybe Text
  corporateShiftId: Maybe Text
  corporateRouteId: Maybe Text
  corporatePolicyId: Maybe Text
  corporateBillingModel: Maybe Text
```

### Search.hs Changes

In `Domain/Action/UI/Search.hs`:
1. If the search request includes corporate fields, validate the employee's active status and policy compliance.
2. Build `CorporateBookingContext` from the validated IDs.
3. Pass context through to BECKN ACL which serializes to `fulfillment.tags`.
4. The `TripCategory` itself remains the standard one (e.g., `OneWay OneWayOnDemand`).

### Confirm.hs Changes

In `Domain/Action/UI/Confirm.hs`:
1. If `Booking` has corporate fields, run corporate policy validation (fare cap, tier check, approval check).
2. If policy requires approval and fare exceeds threshold, create a `CorporateApprovalRequest` and hold the booking.
3. On confirmation, debit corporate wallet via `CorporateBilling.debitCorporateWallet`.

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
      endpoint: /corporate/ride/{rideId}/confirmAttendance
      auth: TokenAuth
      response:
        type: APISuccess

  # Mark leave (cancel scheduled ride)
  - POST:
      endpoint: /corporate/ride/{rideId}/markLeave
      auth: TokenAuth
      response:
        type: APISuccess

  # Submit approval request for on-demand ride
  - POST:
      endpoint: /corporate/approval/request
      auth: TokenAuth
      request:
        type: CorporateApprovalReq
      response:
        type: CorporateApprovalResp

  # Approve/reject (dashboard)
  - POST:
      endpoint: /corporate/approval/{approvalId}/decide
      auth: DashboardAuth
      request:
        type: ApprovalDecisionReq
      response:
        type: APISuccess
```

---

## 4. Key Business Logic Modules

### New: `SharedLogic/CorporateRouteOptimizer.hs`

Route optimization engine using Google Maps Directions API with waypoints:

```
Algorithm (MVP -- Google Maps waypoint API, not custom TSP solver):
1. Input: List of employee addresses + office location + shift time + vehicle capacity
2. Stop clustering: Group nearby employees into pickup stops (K-means on lat/lon, max walk distance 200m)
3. Route generation: Call Google Maps Directions API with up to 25 waypoints per request, optimize=true
4. Multi-route splitting: If employees exceed single vehicle capacity, partition into clusters and generate separate routes
5. Gender safety: Ensure no female first-pickup/last-drop in night shifts; reorder if needed
6. Output: Optimized routes with stops, sequence, timing, assigned vehicles
```

For MVP, we use Google Maps waypoint optimization (which solves small TSP instances). A custom solver (OR-Tools, Concorde) is a future enhancement for 25+ stop routes.

Reuses:
- `Tools/Maps.hs` -- getDistance, getRoutes (Google Maps integration already exists)
- `lib/location-updates` -- distance calculations
- `SharedLogic/Serviceability.hs` -- area checks

### New: `SharedLogic/CorporateScheduler.hs`

Generates bookings from daily roster. Runs as a scheduled job via `lib/scheduler`.

```
Scheduling flow:
1. Trigger: T-12h before shift start (e.g., 8pm for 8am shift)
2. Load: Active roster entries for next day, filter out ON_LEAVE and holidays (CorporateHolidayCalendar)
3. Pre-assign drivers: For each route, select drivers from CorporateSupplyPool (curated pool)
   - Priority: drivers who regularly serve this route > highest rated > nearest
   - If curated pool insufficient, fall back to marketplace (standard allocator)
4. Create SearchRequest per route with CorporateBookingContext fields populated
5. Schedule via lib/scheduler ScheduleJob (same infra as existing scheduled rides)
6. Allocator picks up, respects corporate pool priority
7. Notify assigned drivers (FCM) and employees (push + SMS)
```

Reuses:
- `lib/scheduler/src/Lib/Scheduler/ScheduleJob.hs` -- job scheduling
- `SharedLogic/Allocator/Jobs/ScheduledRides/` -- scheduled ride framework

### Replacement Dispatch Flow

When a vehicle breaks down mid-route:

```
1. Driver/ops reports breakdown -> Roster entry status set to VEHICLE_BREAKDOWN
2. System creates replacement SearchRequest with same CorporateBookingContext
3. New booking ID stored in roster.replacementBookingId
4. Priority allocation: search curated pool first, then marketplace with corporate surge cap
5. Affected employees notified with new driver/vehicle details and revised ETA
6. Original trip billed up to breakdown point; replacement trip billed from pickup
```

### Approval Workflow

For on-demand rides that exceed policy thresholds:

```
1. Employee initiates on-demand corporate ride
2. Search.hs detects fare estimate > policy.approvalThreshold
3. CorporateApprovalRequest created with status PENDING, expiresAt = now + 30min
4. Notification sent to employee's reportingManagerEmail
5. Manager approves/rejects via dashboard API
6. If APPROVED: booking proceeds, wallet debited
7. If REJECTED/EXPIRED: booking cancelled, employee notified
8. AUTO_APPROVED: if policy allows auto-approve for certain conditions (e.g., night shift safety)
```

### New: `SharedLogic/CorporateBilling.hs`

Corporate billing logic:

```
Functions:
- debitCorporateWallet: Real-time per-trip wallet debit on ride completion
  - Atomic: uses DB transaction to debit wallet + create ledger entry
  - If balance < minBalanceThreshold: emit Kafka event for top-up alert
  - If balance <= 0 and autoTopUpEnabled: trigger Juspay auto-debit
- calculateTripCost: GPS-km based fare OR flat route rate OR policy cap (whichever applies)
- generateTripSheet: Daily trip sheet with all completed rides per entity
- reconcileBilling: Match trips to corporate policy, flag anomalies
- generateInvoice: Billing-cycle invoice with GST breakdown (CGST/SGST/IGST based on placeOfSupply)
- issueCreditNote: For disputed/cancelled trips, issue CorporateCreditNote against invoice
- makerCheckerApproval: Two-level approval for disputed trips
```

Reuses:
- `SharedLogic/FareCalculator.hs` -- base fare calculation
- `lib/payment/` -- Juspay integration
- `lib/finance-kernel/` -- HighPrecMoney, currency handling
- `Domain/Action/UI/Invoice.hs` -- invoice generation patterns

### Bulk Upload Geocoding Pipeline

Employee bulk upload (CSV) with address geocoding:

```
1. Dashboard uploads CSV via /corporate/{entityId}/employees/bulkUpload
2. Sync validation: parse CSV, validate required fields, check duplicates
3. Create CorporateEmployee records with defaultPickupLat/Lon = 0
4. Schedule async geocoding job via lib/scheduler:
   - For each employee with address but no coordinates:
     - Call Google Maps Geocoding API (via Tools/Maps.hs)
     - Update defaultPickupLat/Lon
   - Rate-limited: 50 QPS to stay within API quota
   - Failures logged, retried up to 3 times
5. On completion: Kafka event `corporate-employee-geocoded` with success/failure counts
```

### Modify: `SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/DriverPool.hs`

Add corporate supply pool filter:

```
Changes:
- Add CorporateSupplyPool filter: if SearchRequest has corporateEntityId, only include drivers in that entity's curated pool
- Quality tier filter: min rating, max vehicle age, verified BGC
- Vehicle tier filter: match corporate policy's allowed tiers
- Priority: corporate pool drivers get first priority; if insufficient, fall back to marketplace
- Surge cap: apply corporate surge cap from policy (e.g., max 1.2x)
```

### Modify: FareCalculator (BPP side)

**Path**: `app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/FareCalculator.hs`

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
| **Routing** | `Kernel/Utils/CorporateRouting.hs`: stop clustering, distance matrix utils | `SharedLogic/CorporateRouteOptimizer.hs`: full pipeline, Google Maps waypoint API calls, gender safety, multi-route splitting |
| **Billing** | `Kernel/Utils/CorporateBilling.hs`: fare primitives, GPS-km formulas, GST calculation helpers | `SharedLogic/CorporateBilling.hs`: trip-sheet generation, invoice lifecycle, wallet debit orchestration, Juspay integration |

---

## 5. State Machine Transition Diagrams

### CorporateEntity Status

```
ONBOARDING ──(contract signed, config complete)──> ACTIVE
ACTIVE ──(payment default / policy violation)──> SUSPENDED
SUSPENDED ──(issue resolved, payment cleared)──> ACTIVE
ACTIVE ──(contract ended / churned)──> CHURNED
SUSPENDED ──(no resolution within 90d)──> CHURNED
CHURNED ──(re-onboarding)──> ONBOARDING
```

Valid transitions:
- `ONBOARDING -> ACTIVE`
- `ACTIVE -> SUSPENDED`
- `ACTIVE -> CHURNED`
- `SUSPENDED -> ACTIVE`
- `SUSPENDED -> CHURNED`
- `CHURNED -> ONBOARDING`

### AttendanceStatus (CorporateRoster)

```
SCHEDULED ──(employee confirms T-2h)──> CONFIRMED
SCHEDULED ──(employee marks leave)──> ON_LEAVE
SCHEDULED ──(no confirmation by T-30m)──> NO_SHOW
CONFIRMED ──(ride completed)──> COMPLETED
CONFIRMED ──(employee cancels)──> CANCELLED
CONFIRMED ──(vehicle breakdown)──> VEHICLE_BREAKDOWN
VEHICLE_BREAKDOWN ──(replacement dispatched + completed)──> COMPLETED
VEHICLE_BREAKDOWN ──(no replacement available)──> CANCELLED
```

Valid transitions:
- `SCHEDULED -> CONFIRMED`
- `SCHEDULED -> ON_LEAVE`
- `SCHEDULED -> NO_SHOW`
- `CONFIRMED -> COMPLETED`
- `CONFIRMED -> CANCELLED`
- `CONFIRMED -> VEHICLE_BREAKDOWN`
- `VEHICLE_BREAKDOWN -> COMPLETED`
- `VEHICLE_BREAKDOWN -> CANCELLED`

### InvoiceStatus (CorporateInvoice)

```
DRAFT ──(billing cycle ends, invoice generated)──> PENDING_APPROVAL
PENDING_APPROVAL ──(finance team approves)──> APPROVED
PENDING_APPROVAL ──(dispute raised)──> DISPUTED
APPROVED ──(sent to corporate)──> SENT
SENT ──(payment received)──> PAID
SENT ──(past due date)──> OVERDUE
SENT ──(dispute raised)──> DISPUTED
OVERDUE ──(payment received)──> PAID
OVERDUE ──(dispute raised)──> DISPUTED
DISPUTED ──(resolved, credit note issued)──> CREDITED
DISPUTED ──(resolved, no adjustment)──> SENT
```

### WalletStatus (CorporateWallet)

```
ACTIVE ──(balance < 0 or compliance issue)──> FROZEN
FROZEN ──(top-up received or issue resolved)──> ACTIVE
ACTIVE ──(contract ended)──> CLOSED
FROZEN ──(contract ended)──> CLOSED
```

Valid transitions:
- `ACTIVE -> FROZEN`
- `ACTIVE -> CLOSED`
- `FROZEN -> ACTIVE`
- `FROZEN -> CLOSED`

---

## 6. ClickHouse Analytics Schema

Corporate analytics events are pushed via Kafka and consumed into ClickHouse for dashboards and reporting.

### corporate_trip_events

```sql
CREATE TABLE corporate_trip_events
(
    event_id          String,
    event_timestamp   DateTime64(3),
    corporate_entity_id String,
    corporate_employee_id String,
    booking_id        String,
    ride_id           String,
    shift_id          Nullable(String),
    route_id          Nullable(String),
    trip_category     String,
    billing_model     String,
    vehicle_tier      String,
    pickup_lat        Float64,
    pickup_lon        Float64,
    drop_lat          Float64,
    drop_lon          Float64,
    distance_meters   Int64,
    duration_seconds  Int64,
    fare_amount       Decimal64(2),
    surge_multiplier  Float64,
    policy_cap_applied Bool,
    currency          String,
    driver_id         String,
    driver_rating     Float64,
    attendance_status String,
    is_replacement    Bool,
    city              String,
    merchant_id       String
)
ENGINE = MergeTree()
PARTITION BY toYYYYMM(event_timestamp)
ORDER BY (corporate_entity_id, event_timestamp)
TTL event_timestamp + INTERVAL 24 MONTH;
```

### corporate_billing_events

```sql
CREATE TABLE corporate_billing_events
(
    event_id          String,
    event_timestamp   DateTime64(3),
    corporate_entity_id String,
    event_type        Enum8('WALLET_DEBIT' = 1, 'WALLET_CREDIT' = 2, 'WALLET_TOPUP' = 3,
                            'INVOICE_GENERATED' = 4, 'INVOICE_PAID' = 5, 'CREDIT_NOTE' = 6,
                            'THRESHOLD_ALERT' = 7),
    reference_id      String,
    amount            Decimal64(2),
    balance_after     Decimal64(2),
    currency          String,
    invoice_id        Nullable(String),
    wallet_id         Nullable(String),
    billing_model     String,
    period_start      Nullable(DateTime64(3)),
    period_end        Nullable(DateTime64(3)),
    merchant_id       String
)
ENGINE = MergeTree()
PARTITION BY toYYYYMM(event_timestamp)
ORDER BY (corporate_entity_id, event_timestamp)
TTL event_timestamp + INTERVAL 36 MONTH;
```

### corporate_safety_events

```sql
CREATE TABLE corporate_safety_events
(
    event_id          String,
    event_timestamp   DateTime64(3),
    corporate_entity_id String,
    corporate_employee_id String,
    booking_id        String,
    incident_type     Enum8('SOS_TRIGGERED' = 1, 'ROUTE_DEVIATION' = 2, 'UNSCHEDULED_STOP' = 3,
                            'NIGHT_SAFETY_VIOLATION' = 4, 'DRIVER_BEHAVIOR' = 5, 'SAFE_DROP_CONFIRMED' = 6,
                            'SAFE_DROP_FAILED' = 7, 'ESCORT_ASSIGNED' = 8),
    severity          Enum8('LOW' = 1, 'MEDIUM' = 2, 'HIGH' = 3, 'CRITICAL' = 4),
    gps_lat           Nullable(Float64),
    gps_lon           Nullable(Float64),
    driver_id         Nullable(String),
    shift_id          Nullable(String),
    is_night_shift    Bool,
    is_women_safety   Bool,
    resolution_status String,
    resolved_at       Nullable(DateTime64(3)),
    city              String,
    merchant_id       String
)
ENGINE = MergeTree()
PARTITION BY toYYYYMM(event_timestamp)
ORDER BY (corporate_entity_id, event_timestamp)
TTL event_timestamp + INTERVAL 36 MONTH;
```

---

## 7. Dashboard API Integration

### New: `app/dashboard/rider-dashboard/spec/API/Corporate.yaml`

Dashboard APIs for corporate management with DashboardAuth.

### New: `app/dashboard/provider-dashboard/spec/API/CorporateFleet.yaml`

Fleet management APIs for corporate supply pool.

### New: `app/dashboard/CommonAPIs/spec/API/CorporateTypes.yaml`

Shared corporate API types.

---

## 8. Database Migrations

All migrations go in `dev/migrations/rider-app/`:

```
0001_create_corporate_entity.sql
0002_create_corporate_employee.sql
0003_create_corporate_shift.sql
0004_create_corporate_route.sql
0005_create_corporate_route_stop.sql
0006_create_corporate_route_stop_assignment.sql
0007_create_corporate_policy.sql
0008_create_corporate_roster.sql
0009_create_corporate_wallet.sql
0010_create_corporate_invoice.sql
0011_create_corporate_credit_note.sql
0012_create_corporate_holiday_calendar.sql
0013_create_corporate_approval_request.sql
0014_create_corporate_ride_feedback.sql
0015_create_corporate_safety_incident.sql
0016_alter_booking_add_corporate_fields.sql
0017_alter_search_request_add_corporate_fields.sql
```

Driver-side migrations in `dev/migrations/dynamic-offer-driver-app/`:
```
0001_create_corporate_supply_pool.sql
```

---

## 9. Configuration (dhall-configs)

Add to `dhall-configs/dev/`:
- Corporate-specific merchant config
- Corporate fare policy defaults
- Corporate allocation config (pool priority, fallback rules)
- Women safety rule config per city
- Scheduler config (T-12h lead time, geocoding rate limits)

---

## 10. Kafka Events

New Kafka topics for corporate event streaming:
- `corporate-trip-completed` -- trip sheet events -> ClickHouse `corporate_trip_events`
- `corporate-billing-event` -- billing/wallet events -> ClickHouse `corporate_billing_events`
- `corporate-safety-event` -- safety incidents -> ClickHouse `corporate_safety_events`
- `corporate-compliance-alert` -- compliance document expiry
- `corporate-roster-change` -- roster updates
- `corporate-employee-geocoded` -- bulk upload geocoding completion

Reuse existing Kafka infrastructure (`lib/producer/`, `app/kafka-consumers/`).

---

## 11. Implementation Sequence

### Sprint 1-2 (Weeks 1-4): Foundation + ClickHouse Schema
- [ ] Create all NammaDSL storage specs (corrected YAML syntax)
- [ ] Run code generator, fix compilation (`cabal build all`)
- [ ] Create database migrations (all tables)
- [ ] Deploy ClickHouse schemas (corporate_trip_events, corporate_billing_events, corporate_safety_events)
- [ ] Implement CorporateEntity CRUD (dashboard APIs)
- [ ] Implement CorporateEmployee CRUD + bulk upload with async geocoding pipeline
- [ ] Implement CorporateHolidayCalendar CRUD
- [ ] Define BECKN tags in Tags.hs
- [ ] Set up Kafka topics

### Sprint 3-4 (Weeks 5-8): Shifts, Routes + Basic Billing (parallel tracks)

**Track A -- Shifts & Routes:**
- [ ] Implement CorporateShift CRUD
- [ ] Build CorporateRouteOptimizer (Google Maps waypoint API for MVP)
- [ ] Implement CorporateRoute + CorporateRouteStop + CorporateRouteStopAssignment CRUD
- [ ] Build route optimization API endpoint
- [ ] Dashboard APIs for shift and route management

**Track B -- Wallet & Per-trip Billing:**
- [ ] Implement CorporateWallet CRUD + Juspay integration for top-up
- [ ] Implement CorporatePolicy CRUD
- [ ] Build `debitCorporateWallet` (atomic per-trip debit with ledger entry)
- [ ] Build wallet threshold alerts (Kafka events)
- [ ] Wire up billing events to ClickHouse

### Sprint 5-6 (Weeks 9-12): Booking Flow + Scheduling + Safety
- [ ] Add corporate context fields to SearchRequest.yaml and Booking.yaml
- [ ] Modify Search.hs: validate corporate employee/policy, build CorporateBookingContext
- [ ] Modify Confirm.hs: corporate policy validation, wallet debit on completion
- [ ] Build BECKN ACL: serialize/deserialize CorporateBookingContext via fulfillment.tags
- [ ] Build CorporateScheduler: T-12h pre-assignment from curated pool, marketplace fallback
- [ ] Implement CorporateRoster CRUD + attendance confirmation flow
- [ ] Build replacement dispatch flow (VEHICLE_BREAKDOWN -> new booking)
- [ ] Implement safety rules in routing (gender-aware stop ordering for night shifts)
- [ ] Implement CorporateSupplyPool on driver-app side
- [ ] Modify DriverPool.hs for corporate pool priority + fallback

### Sprint 7-8 (Weeks 13-16): Invoice + GST + Ops Desk + Approval
- [ ] Build invoice generation with GST breakdown (CGST/SGST/IGST, SAC code, place of supply)
- [ ] Implement e-invoice IRN generation
- [ ] Implement CorporateCreditNote for disputed/cancelled trips
- [ ] Build daily trip-sheet generation
- [ ] Build ops desk APIs: incident creation (CorporateSafetyIncident), manual dispatch override
- [ ] Implement approval workflow: CorporateApprovalRequest for above-threshold on-demand rides
- [ ] Wire up all Kafka events (trip, billing, safety, compliance, roster)
- [ ] Build CorporateRideFeedback collection

### Sprint 9-10 (Weeks 17-20): Analytics, Compliance, Polish
- [ ] Build ClickHouse analytics queries: per-entity dashboards, trip trends, billing summaries
- [ ] Build compliance reports: safety incident summary, driver BGC status, vehicle age compliance
- [ ] Implement dead-km tracking analytics (empty-leg distance for cost optimization)
- [ ] Driver welfare/training record tracking on BPP side
- [ ] Feedback aggregation and reporting
- [ ] API integration testing (end-to-end corporate ride lifecycle)
- [ ] Performance optimization (query plans, caching hot paths)
- [ ] Dashboard polish: CorporateAnalytics.yaml, CorporateSafety.yaml, CorporateEmployeeUI.yaml API specs
