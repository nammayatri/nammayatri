# SOS System: External ERSS Integration Documentation

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Shared Kernel: External SOS Module](#shared-kernel-external-sos-module)
4. [Rider App Integration](#rider-app-integration)
5. [Dashboard Integration](#dashboard-integration)
6. [Configuration](#configuration)
7. [Database Schema](#database-schema)
8. [API Reference](#api-reference)
9. [Flows](#flows)
10. [Error Handling](#error-handling)

---

## Overview

The SOS system provides emergency response functionality for riders. It integrates with external government emergency services (ERSS, GJ112) to dispatch real-time SOS alerts along with rider location, ride context, and emergency contact information.

**Supported External Providers:**
- **ERSS** (Emergency Response Support System) - C-DAC's national emergency response platform
- **GJ112** - Gujarat 112 emergency service

**Key Capabilities:**
- Send initial SOS signal with rider/ride/vehicle details to external providers
- Send continuous location traces (ERSS only; GJ112 returns success as no-op)
- Update SOS status at the external provider
- Store the external provider's `trackingId` as `externalReferenceId` on the SOS record for subsequent trace/status calls

---

## Architecture

```
Rider App (UI)                    Dashboard
     |                                |
     v                                v
Domain.Action.UI.Sos          Domain.Action.Dashboard.Sos
     |                                |
     +----------- shared ------------ +
                    |
       Kernel.External.SOS.Interface
        (sendInitialSOS, sendSOSTrace, updateSOSStatus)
                    |
         +--------------------+
         |                    |
   Interface.ERSS       Interface.GJ112
         |                    |
     ERSS.Flow           GJ112.Flow
     ERSS.Auth           GJ112.Auth
     ERSS.API            GJ112.API
     ERSS.Types          GJ112.Types
     ERSS.Config         GJ112.Config
```

The integration follows a **unified interface pattern**: all providers implement the same three operations (`sendInitialSOS`, `sendSOSTrace`, `updateSOSStatus`) with shared request/response types. Provider-specific adapters convert these to wire-format types for each external API.

---

## Shared Kernel: External SOS Module

**Path:** `shared-kernel/lib/mobility-core/src/Kernel/External/SOS/`

### Module Structure

| File | Purpose |
|------|---------|
| `Types.hs` | `SOSService` enum (`ERSS`, `GJ112`) |
| `Interface.hs` | Dispatcher: routes calls to provider adapters |
| `Interface/Types.hs` | Unified request/response types |
| `Interface/ERSS.hs` | ERSS adapter (type conversion) |
| `Interface/GJ112.hs` | GJ112 adapter (type conversion) |
| `ERSS/Config.hs` | ERSS config (`ERSSCfg`) |
| `ERSS/Types.hs` | ERSS wire types, error types |
| `ERSS/Auth.hs` | OAuth token management (Keycloak) |
| `ERSS/Flow.hs` | ERSS API call orchestration |
| `ERSS/API.hs` | Servant API type definitions |
| `GJ112/Config.hs` | GJ112 config (`GJ112Cfg`) |
| `GJ112/Types.hs` | GJ112 wire types, error types |
| `GJ112/Auth.hs` | Token management (Base64 credentials) |
| `GJ112/Flow.hs` | GJ112 API call orchestration |
| `GJ112/API.hs` | Servant API type definitions |

### Unified Interface Types

**`SOSServiceConfig`** - Configuration sum type:
```haskell
data SOSServiceConfig
  = ERSSConfig ERSSCfg
  | GJ112Config GJ112Cfg
```

**`InitialSOSReq`** - Sent when SOS is first triggered:

| Field | Type | Description |
|-------|------|-------------|
| `sosId` | `Maybe Text` | Optional source-generated ID |
| `dateTime` | `Text` | Timestamp `"YYYY-MM-DD HH:MM:SS"` |
| `latitude` | `Double` | User's latitude |
| `longitude` | `Double` | User's longitude |
| `speed` | `Maybe Double` | Speed if available |
| `mobileNo` | `Text` | User's mobile number |
| `imeiNo` | `Maybe Text` | Device IMEI |
| `senderName` | `Maybe Text` | User's name |
| `address` | `Maybe Text` | User's address |
| `gpsAccuracy` | `Maybe Double` | GPS accuracy in meters |
| `gender` | `Maybe Text` | `"MALE"` / `"FEMALE"` / `"OTHERS"` |
| `driverName` | `Maybe Text` | Driver name (ride context) |
| `driverContactNo` | `Maybe Text` | Driver phone number |
| `vehicleNo` | `Maybe Text` | Vehicle registration plate |
| `vehicleModel` | `Maybe Text` | Vehicle model |
| `vehicleColor` | `Maybe Text` | Vehicle color |
| `vehicleType` | `Maybe Text` | Vehicle category |
| `vehicleLocationUrl` | `Maybe Text` | Vehicle tracking URL |
| `emergencyContact1Name` | `Maybe Text` | Primary emergency contact name |
| `emergencyContact1Phone` | `Maybe Text` | Primary emergency contact phone |
| `emergencyContact2Name` | `Maybe Text` | Secondary emergency contact name |
| `emergencyContact2Phone` | `Maybe Text` | Secondary emergency contact phone |
| `city` | `Maybe Text` | City of event |
| `emergencyMessage` | `Maybe Text` | Context message |
| `vendorName` | `Maybe Text` | Platform name (e.g. "Namma Yatri") |
| `deviceType` | `Maybe Int` | Device type enum |

**`InitialSOSRes`** - Response from initial SOS:

| Field | Type | Description |
|-------|------|-------------|
| `success` | `Bool` | Whether the call succeeded |
| `trackingId` | `Maybe Text` | Tracking ID for subsequent trace/status calls |
| `errorMessage` | `Maybe Text` | Error description on failure |

**`SOSTraceReq`** - Sent for location updates:

| Field | Type | Description |
|-------|------|-------------|
| `trackingId` | `Text` | From `InitialSOSRes.trackingId` |
| `dateTime` | `Text` | Timestamp `"YYYY-MM-DD HH:MM:SS"` |
| `latitude` | `Double` | Updated latitude |
| `longitude` | `Double` | Updated longitude |
| `speed` | `Maybe Double` | Speed if available |
| `gpsAccuracy` | `Maybe Double` | GPS accuracy in meters |

**`SOSTraceRes`** - Response from trace:

| Field | Type | Description |
|-------|------|-------------|
| `success` | `Bool` | Whether the call succeeded |
| `errorMessage` | `Maybe Text` | Error description on failure |

**`SOSStatusUpdateReq`** / **`SOSStatusUpdateRes`** - For status updates (same pattern).

### ERSS Provider Details

**Authentication:** OAuth 2.0 via Keycloak (password grant + refresh token grant).

- Auth endpoint: `POST /realms/ngerss/protocol/openid-connect/token`
- Tokens cached in Redis (`Core:erss_token` key prefix)
- Access token auto-refreshed when expired; falls back to password grant when refresh token expires
- 60-second buffer subtracted from token TTL for safety

**API Endpoints:**

| Operation | Method | Path | Auth |
|-----------|--------|------|------|
| Initial SOS | `POST` | `/public/api/sos/initial` | `Bearer {token}` |
| SOS Trace | `POST` | `/public/api/sos/trace` | `Bearer {token}` |
| Status Update | `POST` | `/public/api/sos/status` | `Bearer {token}` |

**ERSS Request Fields** (in addition to unified fields):

| Field | Type | Description |
|-------|------|-------------|
| `authId` | `Text` | Source identifier (from config) |
| `authCode` | `Text` | Source verification code (from config) |
| `stateCode` | `Maybe Text` | State code assigned by C-DAC |
| `silentCommunication` | `Maybe Text` | `"true"`/`"false"` - can operator call? |

**ERSS Response Envelope:**
```json
{
  "resultCode": "OPERATION_SUCCESS" | "OPERATION_FAILURE",
  "resultString": "...",
  "errorMsg": "...",
  "message": "...",
  "payLoad": { "signalId": 12345, "status": "STARTED", "remarks": "..." }
}
```

The `signalId` from `payLoad` is converted to `trackingId` in the unified response.

**Error Handling:**
- `ERSSOperationFailure` - API returned `OPERATION_FAILURE`
- `ERSSAuthError` - Authentication/Keycloak errors
- `ERSSUnknownError` - Unparseable errors
- Response validation: even HTTP 200 responses are checked for `OPERATION_FAILURE` in the body

**Configuration (`ERSSCfg`):**

| Field | Type | Description |
|-------|------|-------------|
| `baseUrl` | `BaseUrl` | ERSS base URL (e.g. `https://lbn.erss.in`) |
| `clientId` | `Text` | OAuth client ID |
| `clientSecret` | `EncryptedField` | OAuth client secret (encrypted) |
| `username` | `EncryptedField` | ERSS username (encrypted) |
| `password` | `EncryptedField` | ERSS password (encrypted) |
| `authId` | `Text` | Source identifier for API requests |
| `authCode` | `Text` | Source verification code for API requests |
| `tokenKeyPrefix` | `Text` | Redis key prefix for token caching |

### GJ112 Provider Details

**Authentication:** Base64-encoded credentials sent to auth endpoint.

- Auth endpoint: `POST /api/authentication/no-auth/validateUser`
- Tokens cached in Redis (`Core:gj112_token` key prefix)
- Token includes `"Bearer "` prefix from the server response

**API Endpoints:**

| Operation | Method | Path | Auth |
|-----------|--------|------|------|
| Send SOS | `POST` | `/api/citizen/saveSosPanicRequests` | Token header |

**GJ112 Limitations:**
- `sendSOSTrace` is a **no-op** (returns `success = True`) - GJ112 does not support location traces
- `updateSOSStatus` is a **no-op** (returns `success = True`) - GJ112 does not support status updates

**GJ112 Request** includes ride-hailing context fields: `driverName`, `driverContactNo`, `vehicleNo`, `vehicleModel`, `vehLat`, `vehLng`, `vehLocUrl`, `vendorName`, `vehicleColor`, `vehicleType`, `vehicleMake`, `vehicleAppearanceNotes`.

**GJ112 Response:**
```json
{
  "message": "...",
  "responseCode": 200,
  "referenceId": 12345,
  "action": "Dispatch notified"
}
```

The `referenceId` is converted to `trackingId` (as string) in the unified response. Success is determined by `responseCode == 200`.

**Configuration (`GJ112Cfg`):**

| Field | Type | Description |
|-------|------|-------------|
| `apiUrl` | `BaseUrl` | GJ112 API base URL |
| `userName` | `EncryptedField` | Username (encrypted, Base64-encoded on use) |
| `password` | `EncryptedField` | Password (encrypted, Base64-encoded on use) |
| `clientId` | `Text` | Unique client identifier |
| `clientCode` | `Text` | Client short code |
| `tokenKeyPrefix` | `Text` | Redis key prefix for token caching |

---

## Rider App Integration

**Path:** `Backend/app/rider-platform/rider-app/Main/`

### Key Files

| File | Purpose |
|------|---------|
| `src/Domain/Action/UI/Sos.hs` | Core SOS business logic |
| `src/Domain/Action/Dashboard/Sos.hs` | Dashboard-triggered SOS operations |
| `src-read-only/API/Types/UI/Sos.hs` | API request/response types |
| `src-read-only/API/Action/UI/Sos.hs` | API route definitions |
| `spec/Storage/sos.yaml` | Database schema spec |
| `src-read-only/Domain/Types/Sos.hs` | Domain types |
| `src-read-only/Storage/Queries/Sos.hs` | Database queries |
| `src/Storage/CachedQueries/Sos.hs` | Redis caching layer |
| `src/SharedLogic/SosLocationTracking.hs` | Location tracking in Redis |
| `src/SharedLogic/Scheduler/Jobs/SafetyCSAlert.hs` | Scheduled safety alerts |

### External SOS in `postSosCreate`

**File:** `src/Domain/Action/UI/Sos.hs`

When a rider triggers SOS, the system checks `riderConfig.externalSOSConfig`:

1. If `triggerSource == FRONTEND`:
   - Resolves the `SOSServiceConfig` from `MerchantServiceConfig`
   - Builds `InitialSOSReq` via `buildExternalSOSDetails` with:
     - Rider details (name, phone, gender)
     - Current location (lat/lon from `customerLocation`)
     - Ride context (driver name, vehicle info, tracking URL)
     - Emergency contacts (up to 2)
     - Metadata (city, vendor name, timestamp)
   - Calls `PoliceSOS.sendInitialSOS`
   - On failure: throws `InternalError` with the provider's error message
   - On success: extracts `trackingId` as `mbTrackingId`
2. After SOS record creation, stores `trackingId` via `QSos.updateExternalReferenceId`

### External SOS Trace in `postSosUpdateLocation`

**File:** `src/Domain/Action/UI/Sos.hs`

When the rider's location is updated:

1. Updates location in Redis (for internal tracking)
2. Checks `riderConfig.externalSOSConfig`
3. If config exists AND `sosDetails.externalReferenceId` is present:
   - Resolves the `SOSServiceConfig`
   - Builds `SOSTraceReq` with:
     - `trackingId` from `externalReferenceId`
     - Current timestamp
     - Updated lat/lon from request
     - GPS accuracy
   - Calls `PoliceSOS.sendSOSTrace`
   - On failure: throws `InternalError`

### `buildExternalSOSDetails`

**File:** `src/Domain/Action/UI/Sos.hs`

Constructs the unified `InitialSOSReq`:

```
Inputs:
  - SosReq (from rider)
  - Person (rider details)
  - ExternalSOSConfig (provider config)
  - Maybe Ride (ride context)
  - [EmergencyContactAPIEntity] (up to 2 contacts)
  - MerchantOperatingCity
  - RiderConfig

Output: InitialSOSReq with all available context
```

Notable behavior:
- `lat`/`lon` are set to `0.0` if `sosConfig.latLonRequired` is `False` or no location provided
- Emergency contacts: first two contacts' names and phones are extracted
- Vehicle tracking URL built from ride ID if ride is present

---

## Dashboard Integration

**Path:** `Backend/app/dashboard/rider-dashboard/` and `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Dashboard/Sos.hs`

### Dashboard API Endpoints

| Method | Path | Auth | Description |
|--------|------|------|-------------|
| `GET` | `/sos/{sosId}/tracking` | NoAuth | Get SOS tracking status |
| `POST` | `/sos/{sosId}/callExternalSOS` | ApiAuthV2 | Trigger external SOS from dashboard |
| `GET` | `/sos/media/{personId}` | ApiAuthV2 | Get SOS media files |

### Dashboard External SOS (`callExternalSOS`)

**File:** `src/Domain/Action/Dashboard/Sos.hs`

Called when `triggerSource == DASHBOARD` (external SOS is triggered by the dashboard operator, not the rider's device):

1. Looks up the existing SOS record by `sosId`
2. Validates `externalSOSConfig.triggerSource == DASHBOARD`
3. Resolves `SOSServiceConfig` from `MerchantServiceConfig`
4. Builds `InitialSOSReq` using `buildExternalSOSDetails`
5. Calls `PoliceSOS.sendInitialSOS`
6. On failure: throws `InternalError`
7. On success: stores `trackingId` via `QSos.updateExternalReferenceId`

### Dashboard API Type Definitions

**File:** `CommonAPIs/spec/RiderPlatform/Management/API/Sos.yaml`

```yaml
apis:
  - GET:
      endpoint: /{sosId}/tracking
      auth: NoAuth
      response: SosTrackingRes

  - POST:
      endpoint: /{sosId}/callExternalSOS
      auth: ApiAuthV2
      response: APISuccess

types:
  SosTrackingRes:
    - currentLocation: Maybe SosLocationRes
    - sosState: Maybe SosState (LiveTracking | SosActive)
    - status: SosStatus (NotResolved | Pending | Resolved | MockPending | MockResolved)

  SosLocationRes:
    - lat: Double
    - lon: Double
    - accuracy: Maybe Double
```

### Dashboard Request Flow

```
Dashboard UI
  -> rider-dashboard (API.Action.RiderPlatform.Management.Sos)
    -> API.Client.RiderPlatform.Management (Servant client call)
      -> rider-app Dashboard API (API.Action.Dashboard.Management.Sos)
        -> Domain.Action.Dashboard.Sos (callExternalSOS)
          -> Kernel.External.SOS.Interface.sendInitialSOS
            -> ERSS / GJ112 provider
```

The dashboard uses a generated Servant client (`ManagementAPIs.sosDSL`) to call the rider-app's internal dashboard API, which then dispatches to the external SOS provider.

---

## Configuration

### RiderConfig: `externalSOSConfig`

**Type:** `Maybe ExternalSOSConfig`

```haskell
data ExternalSOSConfig = ExternalSOSConfig
  { flow              :: ExternalSOSFlow           -- ERSS | GJ112
  , triggerSource     :: ExternalSOSTriggerSource   -- FRONTEND | DASHBOARD
  , latLonRequired    :: Bool                       -- Include lat/lon in request
  , mediaRequired     :: Bool                       -- (reserved)
  , trackingLinkRequired :: Bool                    -- (reserved)
  }
```

- Set per `MerchantOperatingCity` via `RiderConfig`
- `triggerSource` determines whether the external API is called from the rider app (`FRONTEND`) or the dashboard (`DASHBOARD`)
- If `Nothing`, no external SOS integration is active

### MerchantServiceConfig: `SOSServiceConfig`

The actual provider credentials are stored in `MerchantServiceConfig` under `ServiceName = SOSService ERSS` or `SOSService GJ112`:

```haskell
data ServiceConfigD = ...
  | SOSServiceConfig SOSInterface.SOSServiceConfig
  -- where SOSServiceConfig = ERSSConfig ERSSCfg | GJ112Config GJ112Cfg
```

Looked up at runtime via:
```haskell
QMSC.findByMerchantOpCityIdAndService merchantId merchantOpCityId (DMSC.SOSService sosServiceType)
```

---

## Database Schema

**Table:** `atlas_app.sos`

| Column | Type | Description |
|--------|------|-------------|
| `id` | `varchar(36)` | Primary key |
| `person_id` | `varchar(36)` | Rider ID |
| `ride_id` | `varchar(36)` | Associated ride ID |
| `flow` | `text` | SOS type (Police, SafetyFlow, etc.) |
| `status` | `text` | Resolved, NotResolved, Pending, MockPending, MockResolved |
| `ticket_id` | `text` | Kapture ticket ID |
| `media_files` | `text[]` | Array of media file IDs |
| `tracking_expires_at` | `timestamptz` | When location tracking expires |
| `sos_state` | `text` | LiveTracking or SosActive |
| `entity_type` | `text` | Ride or NonRide |
| `external_reference_id` | `text` | **Tracking ID from external SOS provider** |
| `merchant_id` | `varchar(36)` | Merchant ID |
| `merchant_operating_city_id` | `varchar(36)` | Operating city ID |
| `created_at` | `timestamptz` | Creation timestamp |
| `updated_at` | `timestamptz` | Last update timestamp |

**Key Queries:**

| Query | Description |
|-------|-------------|
| `findById` | Lookup SOS by ID |
| `findByRideId` | Lookup SOS by ride ID |
| `findByTicketId` | Lookup SOS by Kapture ticket ID |
| `findByPersonId` | All SOS records for a person |
| `updateStatus` | Update SOS status |
| `updateExternalReferenceId` | Store external provider's tracking ID |
| `updateState` | Update SOS state (LiveTracking/SosActive) |
| `updateTrackingExpiresAt` | Set tracking expiry |

---

## API Reference

### Rider App UI APIs

**Base path:** `/sos`

| Method | Endpoint | Request | Response | Description |
|--------|----------|---------|----------|-------------|
| `GET` | `/getDetails/{rideId}` | - | `SosDetailsRes` | Get SOS details for a ride |
| `POST` | `/create` | `SosReq` | `SosRes` | Create SOS (triggers external API if configured) |
| `POST` | `/{sosId}/status` | `SosUpdateReq` | `APISuccess` | Update SOS status |
| `POST` | `/{sosId}/markRideAsSafe` | `MarkAsSafeReq` | `APISuccess` | Mark ride as safe |
| `POST` | `/createMockSos` | `MockSosReq` | `APISuccess` | Create mock SOS drill |
| `POST` | `/callPolice` | `CallPoliceAPI` | `APISuccess` | Trigger police call |
| `POST` | `/{sosId}/updateLocation` | `SosLocationUpdateReq` | `APISuccess` | Update location (sends trace to external provider) |
| `GET` | `/{sosId}/tracking` | - | `SosTrackingRes` | Get tracking info |
| `POST` | `/startTracking` | `StartTrackingReq` | `StartTrackingRes` | Start standalone tracking |
| `POST` | `/{sosId}/updateState` | `UpdateStateReq` | `APISuccess` | Update SOS state |
| `GET` | `/{sosId}/trackingDetails` | - | `SosTrackingDetailsRes` | Get person details for tracking |
| `GET` | `/IvrOutcome` | Query params | `APISuccess` | Exotel IVR callback |

### Key Request Types

**`SosReq`** (Create SOS):
```json
{
  "flow": "SafetyFlow",
  "rideId": "ride-uuid",
  "isRideEnded": false,
  "notifyAllContacts": true,
  "customerLocation": { "lat": 12.97, "lon": 77.59 },
  "sendPNOnPostRideSOS": false
}
```

**`SosLocationUpdateReq`** (Update location / send trace):
```json
{
  "lat": 12.97,
  "lon": 77.59,
  "accuracy": 10.5
}
```

---

## Flows

### Flow 1: SOS Creation with External ERSS (Frontend Trigger)

```
1. Rider triggers SOS in app
2. POST /sos/create -> postSosCreate
3. Check riderConfig.externalSOSConfig
4. triggerSource == FRONTEND:
   a. Resolve SOSServiceConfig (ERSSConfig)
   b. Build InitialSOSReq with rider/ride/vehicle/contact details
   c. Call sendInitialSOS -> ERSS OAuth -> POST /public/api/sos/initial
   d. ERSS returns { resultCode: "OPERATION_SUCCESS", payLoad: { signalId: 12345 } }
   e. Extract trackingId = "12345" (from signalId)
   f. If success == False, throw InternalError
5. Create SOS record in DB
6. Store trackingId as externalReferenceId via updateExternalReferenceId
7. Notify emergency contacts
8. Return SosRes { sosId }
```

### Flow 2: Location Trace to External ERSS

```
1. Rider's device sends location update
2. POST /sos/{sosId}/updateLocation -> postSosUpdateLocation
3. Validate ownership and status (must be Pending)
4. Update location in Redis (internal tracking)
5. Check riderConfig.externalSOSConfig exists
6. Check sosDetails.externalReferenceId exists (trackingId from initial SOS)
7. Resolve SOSServiceConfig (ERSSConfig)
8. Build SOSTraceReq:
   { trackingId: "12345", dateTime: "2025-01-15 14:30:00",
     latitude: 12.97, longitude: 77.59, speed: null, gpsAccuracy: 10.5 }
9. Call sendSOSTrace -> ERSS OAuth -> POST /public/api/sos/trace
10. ERSS returns { resultCode: "OPERATION_SUCCESS" }
11. If success == False, throw InternalError
12. Return APISuccess
```

### Flow 3: Dashboard-Triggered External SOS

```
1. Dashboard operator clicks "Call External SOS" for an SOS record
2. POST /sos/{sosId}/callExternalSOS -> postSosCallExternalSOS
3. Rider-dashboard calls rider-app Management API
4. callExternalSOS:
   a. Validate triggerSource == DASHBOARD
   b. Resolve SOSServiceConfig
   c. Build InitialSOSReq from existing SOS record + rider/ride data
   d. Call sendInitialSOS
   e. On success: store trackingId as externalReferenceId
   f. On failure: throw InternalError
5. Return APISuccess
```

### Flow 4: Complete SOS Lifecycle with ERSS

```
                    Rider App                           ERSS
                        |                                 |
  1. Trigger SOS ------>|                                 |
                        |-- sendInitialSOS -------------->|
                        |<-- trackingId=12345 ------------|
                        |                                 |
  2. Store externalReferenceId = "12345"                  |
                        |                                 |
  3. Location update -->|                                 |
                        |-- sendSOSTrace (trackingId) --->|
                        |<-- success ---------------------|
                        |                                 |
  4. Location update -->|                                 |
                        |-- sendSOSTrace (trackingId) --->|
                        |<-- success ---------------------|
                        |                                 |
  5. Mark as safe ----->|                                 |
                        | (updateSOSStatus can be called) |
```

---

## Error Handling

### External API Errors

**On `sendInitialSOS` failure:**
- Throws `InternalError` with the provider's `errorMessage` (or `"External SOS call failed"` as default)
- SOS record is **not** created (for frontend trigger path)
- For dashboard path, the SOS record already exists but `externalReferenceId` remains `Nothing`

**On `sendSOSTrace` failure:**
- Throws `InternalError` with the provider's `errorMessage` (or `"SOS Trace failed"` as default)
- Location is still updated in Redis (internal tracking succeeds independently)

### ERSS-Specific Errors

| Error Type | HTTP Code | When |
|------------|-----------|------|
| `ERSSOperationFailure` | 500 | API returns `OPERATION_FAILURE` in body |
| `ERSSAuthError` | 401 | Keycloak auth fails |
| `ERSSUnknownError` | 500 | Unparseable error response |

ERSS responses are validated even on HTTP 200 - the `resultCode` field in the body is checked for `OPERATION_SUCCESS`.

### GJ112-Specific Errors

| Error Type | HTTP Code | When |
|------------|-----------|------|
| `GJ112Error` | 500 | Any API failure |

GJ112 success is determined by `responseCode == 200` in the response body.

### Graceful Degradation

- If `externalSOSConfig` is `Nothing`, no external calls are made
- If `externalReferenceId` is `Nothing` on the SOS record, trace calls are skipped
- GJ112 trace/status calls are no-ops by design (always return success)
- Internal SOS functionality (notifications, tracking, tickets) works independently of external provider status
